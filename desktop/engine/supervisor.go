package engine

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os/exec"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"time"
)

// State is the lifecycle state of the engine process.
type State string

const (
	StateStopped      State = "stopped"
	StateStarting     State = "starting"
	StateReady        State = "ready"
	StateCrashed      State = "crashed"      // exited while it should have been running
	StateIncompatible State = "incompatible" // handshake succeeded at the transport level but the versions do not match
	StateFailed       State = "failed"       // could not be started or did not complete the handshake
)

// Status describes the engine as last observed by the supervisor.
type Status struct {
	State          State  `json:"state"`
	Detail         string `json:"detail"`
	ServiceVersion string `json:"serviceVersion"`
	Protocol       int    `json:"protocol"`
	Starts         int    `json:"starts"`
}

// Config configures a Supervisor. Command is required.
type Config struct {
	Command string
	Args    []string

	HandshakeTimeout time.Duration // default 10s
	ShutdownTimeout  time.Duration // default 3s
	MaxMessageBytes  int           // largest accepted engine message, default 64 MiB

	// Callbacks run on supervisor goroutines and must not block for long.
	OnStatus func(Status)
	OnEvent  func(name string, data json.RawMessage)
	Log      func(line string)
}

// Supervisor runs at most one engine process at a time.
type Supervisor struct {
	cfg       Config
	lifecycle sync.Mutex // serializes Start, Stop, and Restart

	mu     sync.Mutex
	status Status
	proc   *process
}

// reservedMethods are owned by the supervisor's lifecycle handling.
var reservedMethods = map[string]bool{"hello": true, "shutdown": true, "cancel": true}

func New(cfg Config) *Supervisor {
	if cfg.HandshakeTimeout <= 0 {
		cfg.HandshakeTimeout = 10 * time.Second
	}
	if cfg.ShutdownTimeout <= 0 {
		cfg.ShutdownTimeout = 3 * time.Second
	}
	if cfg.MaxMessageBytes <= 0 {
		cfg.MaxMessageBytes = 64 << 20
	}
	if cfg.Log == nil {
		cfg.Log = func(string) {}
	}
	return &Supervisor{cfg: cfg, status: Status{State: StateStopped}}
}

// Status returns the current engine status.
func (s *Supervisor) Status() Status {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.status
}

// Start launches the engine and performs the handshake. It is a no-op if the
// engine is already running.
func (s *Supervisor) Start(ctx context.Context) error {
	s.lifecycle.Lock()
	defer s.lifecycle.Unlock()
	return s.startLocked(ctx)
}

// Stop shuts the engine down: an orderly shutdown request, then closing its
// input, then killing it if it has not exited within the shutdown timeout.
func (s *Supervisor) Stop() error {
	s.lifecycle.Lock()
	defer s.lifecycle.Unlock()
	s.stopLocked()
	return nil
}

// Restart stops any running engine and starts a new one.
func (s *Supervisor) Restart(ctx context.Context) error {
	s.lifecycle.Lock()
	defer s.lifecycle.Unlock()
	s.stopLocked()
	return s.startLocked(ctx)
}

// Call sends a request and waits for its result. Engine errors are returned
// as *RemoteError. If ctx is cancelled the engine is asked to cancel the
// request and ctx.Err() is returned.
func (s *Supervisor) Call(ctx context.Context, method string, params json.RawMessage) (json.RawMessage, error) {
	if reservedMethods[method] {
		return nil, &RemoteError{Code: "method_not_allowed", Message: "Method is managed by the host: " + method}
	}
	s.mu.Lock()
	p := s.proc
	ready := s.status.State == StateReady
	s.mu.Unlock()
	if p == nil || !ready {
		return nil, ErrUnavailable
	}
	return p.call(ctx, method, params)
}

func (s *Supervisor) setStatus(update func(*Status)) {
	s.mu.Lock()
	update(&s.status)
	st := s.status
	s.mu.Unlock()
	if s.cfg.OnStatus != nil {
		s.cfg.OnStatus(st)
	}
}

func (s *Supervisor) startLocked(ctx context.Context) error {
	s.mu.Lock()
	running := s.proc != nil
	s.mu.Unlock()
	if running {
		return nil
	}
	s.setStatus(func(st *Status) {
		st.State, st.Detail, st.ServiceVersion, st.Protocol = StateStarting, "", "", 0
		st.Starts++
	})

	p, err := s.spawn()
	if err != nil {
		s.setStatus(func(st *Status) { st.State, st.Detail = StateFailed, err.Error() })
		return err
	}

	hctx, cancel := context.WithTimeout(ctx, s.cfg.HandshakeTimeout)
	raw, err := p.call(hctx, "hello", json.RawMessage(`{"protocols":[`+strconv.Itoa(ProtocolVersion)+`]}`))
	cancel()

	fail := func(state State, detail string) error {
		s.terminate(p)
		s.setStatus(func(st *Status) { st.State, st.Detail = state, detail })
		return errors.New(detail)
	}
	var remote *RemoteError
	switch {
	case errors.As(err, &remote) && remote.Code == "unsupported_version":
		return fail(StateIncompatible, fmt.Sprintf("engine does not support protocol %d: %s", ProtocolVersion, remote.Message))
	case errors.Is(err, context.DeadlineExceeded):
		return fail(StateFailed, "engine did not answer the handshake in time")
	case err != nil:
		return fail(StateFailed, "handshake failed: "+p.describeExit(err))
	}
	var hello helloResult
	if err := json.Unmarshal(raw, &hello); err != nil {
		return fail(StateIncompatible, "unrecognised handshake response")
	}
	if hello.Service != ServiceName || hello.Protocol != ProtocolVersion {
		return fail(StateIncompatible, fmt.Sprintf("expected %s protocol %d, got %q protocol %d",
			ServiceName, ProtocolVersion, hello.Service, hello.Protocol))
	}

	s.mu.Lock()
	s.proc = p
	s.mu.Unlock()
	s.setStatus(func(st *Status) {
		st.State, st.Detail = StateReady, ""
		st.ServiceVersion, st.Protocol = hello.ServiceVersion, hello.Protocol
	})
	go s.watch(p)
	return nil
}

func (s *Supervisor) stopLocked() {
	s.mu.Lock()
	p := s.proc
	s.proc = nil
	s.mu.Unlock()
	if p != nil {
		s.terminate(p)
	}
	s.setStatus(func(st *Status) {
		if st.State != StateIncompatible && st.State != StateFailed && st.State != StateCrashed {
			st.State, st.Detail = StateStopped, ""
		}
	})
}

// watch reports an engine that exits on its own.
func (s *Supervisor) watch(p *process) {
	<-p.exited
	if p.stopping.Load() {
		return
	}
	s.mu.Lock()
	current := s.proc == p
	if current {
		s.proc = nil
	}
	s.mu.Unlock()
	p.release()
	if current {
		s.setStatus(func(st *Status) {
			st.State, st.Detail = StateCrashed, p.describeExit(nil)
		})
	}
}

// terminate stops a process and waits for it, escalating to kill.
func (s *Supervisor) terminate(p *process) {
	p.stopping.Store(true)
	ctx, cancel := context.WithTimeout(context.Background(), s.cfg.ShutdownTimeout)
	_, _ = p.call(ctx, "shutdown", json.RawMessage(`{}`))
	cancel()
	_ = p.stdin.Close()
	select {
	case <-p.exited:
	case <-time.After(s.cfg.ShutdownTimeout):
		s.cfg.Log("engine did not exit after shutdown; killing it")
		_ = p.cmd.Process.Kill()
		<-p.exited
	}
	p.release()
}

// process is one running engine.
type process struct {
	cmd     *exec.Cmd
	stdin   io.WriteCloser
	writeMu sync.Mutex
	log     func(string)

	mu      sync.Mutex
	pending map[string]chan reply
	closed  bool
	nextID  atomic.Uint64

	stopping    atomic.Bool
	exited      chan struct{}
	exitErr     error
	lastStderr  atomic.Value // string
	releaseOnce sync.Once
	releaseJob  func()
}

type reply struct {
	result json.RawMessage
	err    error
}

func (s *Supervisor) spawn() (*process, error) {
	cmd := exec.Command(s.cfg.Command, s.cfg.Args...)
	configureCommand(cmd)
	stdin, err := cmd.StdinPipe()
	if err != nil {
		return nil, err
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, err
	}
	stderr, err := cmd.StderrPipe()
	if err != nil {
		return nil, err
	}
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("starting engine: %w", err)
	}
	release, err := attachJob(cmd)
	if err != nil {
		_ = cmd.Process.Kill()
		_ = cmd.Wait()
		return nil, fmt.Errorf("tying engine lifetime to the host: %w", err)
	}

	p := &process{
		cmd:        cmd,
		stdin:      stdin,
		log:        s.cfg.Log,
		pending:    make(map[string]chan reply),
		exited:     make(chan struct{}),
		releaseJob: release,
	}
	p.lastStderr.Store("")

	var readers sync.WaitGroup
	readers.Add(2)
	go func() {
		defer readers.Done()
		sc := bufio.NewScanner(stderr)
		for sc.Scan() {
			line := sc.Text()
			p.lastStderr.Store(line)
			s.cfg.Log(line)
		}
	}()
	go func() {
		defer readers.Done()
		sc := bufio.NewScanner(stdout)
		sc.Buffer(make([]byte, 64*1024), s.cfg.MaxMessageBytes)
		for sc.Scan() {
			s.handleLine(p, sc.Bytes())
		}
		if err := sc.Err(); err != nil {
			s.cfg.Log("engine output unreadable, stopping engine: " + err.Error())
			_ = cmd.Process.Kill()
		}
	}()
	go func() {
		readers.Wait()
		p.exitErr = cmd.Wait()
		p.mu.Lock()
		p.closed = true
		pending := p.pending
		p.pending = nil
		p.mu.Unlock()
		for _, ch := range pending {
			ch <- reply{err: ErrUnavailable}
		}
		close(p.exited)
	}()
	return p, nil
}

func (s *Supervisor) handleLine(p *process, line []byte) {
	var msg incoming
	if err := json.Unmarshal(line, &msg); err != nil {
		s.cfg.Log("ignoring malformed engine message: " + err.Error())
		return
	}
	if msg.Event != nil {
		if s.cfg.OnEvent != nil {
			s.cfg.OnEvent(*msg.Event, msg.Data)
		}
		return
	}
	if msg.ID == nil {
		if msg.Error != nil {
			s.cfg.Log("engine reported: " + msg.Error.Error())
		}
		return
	}
	p.mu.Lock()
	ch, ok := p.pending[*msg.ID]
	delete(p.pending, *msg.ID)
	p.mu.Unlock()
	if !ok {
		return // e.g. the answer to a cancelled request
	}
	switch {
	case msg.OK != nil && *msg.OK:
		ch <- reply{result: msg.Result}
	case msg.Error != nil:
		ch <- reply{err: msg.Error}
	default:
		ch <- reply{err: &RemoteError{Code: "invalid_response", Message: "engine response had neither result nor error"}}
	}
}

func (p *process) call(ctx context.Context, method string, params json.RawMessage) (json.RawMessage, error) {
	if len(params) == 0 {
		params = json.RawMessage(`{}`)
	}
	id := strconv.FormatUint(p.nextID.Add(1), 10)
	ch := make(chan reply, 1)
	p.mu.Lock()
	if p.closed {
		p.mu.Unlock()
		return nil, ErrUnavailable
	}
	p.pending[id] = ch
	p.mu.Unlock()

	if err := p.send(outgoing{V: ProtocolVersion, ID: id, Method: method, Params: params}); err != nil {
		p.forget(id)
		return nil, ErrUnavailable
	}
	select {
	case r := <-ch:
		return r.result, r.err
	case <-ctx.Done():
		if p.forget(id) {
			cancelParams, _ := json.Marshal(map[string]string{"target": id})
			_ = p.send(outgoing{V: ProtocolVersion, ID: "cancel-" + id, Method: "cancel", Params: cancelParams})
		}
		return nil, ctx.Err()
	}
}

// forget removes a pending request; it reports whether it was still pending.
func (p *process) forget(id string) bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	if _, ok := p.pending[id]; !ok {
		return false
	}
	delete(p.pending, id)
	return true
}

func (p *process) send(msg outgoing) error {
	line, err := json.Marshal(msg)
	if err != nil {
		return err
	}
	p.writeMu.Lock()
	defer p.writeMu.Unlock()
	_, err = p.stdin.Write(append(line, '\n'))
	return err
}

func (p *process) release() {
	p.releaseOnce.Do(p.releaseJob)
}

// describeExit summarises why the process ended, with its last stderr line.
func (p *process) describeExit(cause error) string {
	var parts []string
	select {
	case <-p.exited:
		if p.exitErr != nil {
			parts = append(parts, "engine exited: "+p.exitErr.Error())
		} else {
			parts = append(parts, "engine exited")
		}
	default:
		if cause != nil {
			parts = append(parts, cause.Error())
		}
	}
	if last, _ := p.lastStderr.Load().(string); last != "" {
		parts = append(parts, "last log: "+last)
	}
	return strings.Join(parts, "; ")
}
