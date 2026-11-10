package engine

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"
)

// The test binary doubles as a scriptable fake engine: when
// SONORA_FAKE_ENGINE is set it speaks the protocol on stdin/stdout.
func TestMain(m *testing.M) {
	switch mode := os.Getenv("SONORA_FAKE_ENGINE"); mode {
	case "":
		os.Exit(m.Run())
	case "host":
		runFakeHost()
	default:
		runFakeEngine(mode)
	}
}

func runFakeEngine(mode string) {
	fmt.Fprintln(os.Stderr, "[fake-engine] started in mode", mode)
	out := bufio.NewWriter(os.Stdout)
	var outMu sync.Mutex
	write := func(v any) {
		b, _ := json.Marshal(v)
		outMu.Lock()
		out.Write(append(b, '\n'))
		out.Flush()
		outMu.Unlock()
	}
	ok := func(id string, result any) { write(map[string]any{"v": 1, "id": id, "ok": true, "result": result}) }
	fail := func(id any, code, msg string) {
		write(map[string]any{"v": 1, "id": id, "ok": false, "error": map[string]any{"code": code, "message": msg}})
	}

	var mu sync.Mutex
	cancels := map[string]chan struct{}{}
	sc := bufio.NewScanner(os.Stdin)
	for sc.Scan() {
		var req struct {
			ID     string          `json:"id"`
			Method string          `json:"method"`
			Params json.RawMessage `json:"params"`
		}
		if err := json.Unmarshal(sc.Bytes(), &req); err != nil {
			fail(nil, "parse_error", err.Error())
			continue
		}
		switch req.Method {
		case "hello":
			switch mode {
			case "silent":
				continue
			case "incompatible":
				fail(req.ID, "unsupported_version", "No mutually supported protocol version")
				continue
			case "wrong-service":
				ok(req.ID, map[string]any{"protocol": 1, "service": "something-else", "serviceVersion": "9"})
				continue
			}
			ok(req.ID, map[string]any{"protocol": 1, "service": "sonora-service", "serviceVersion": "0.0.0-fake",
				"methods": []string{"hello", "ping"}})
		case "ping":
			if mode == "crash-on-ping" {
				fmt.Fprintln(os.Stderr, "[fake-engine] crashing as requested")
				os.Exit(3)
			}
			ok(req.ID, map[string]any{})
		case "echo":
			ok(req.ID, req.Params)
		case "fail":
			fail(req.ID, "invalid_params", "bad input")
		case "emit":
			write(map[string]any{"v": 1, "event": "progress", "data": req.Params})
			ok(req.ID, map[string]any{})
		case "sleep":
			done := make(chan struct{})
			mu.Lock()
			cancels[req.ID] = done
			mu.Unlock()
			go func(id string) {
				select {
				case <-time.After(5 * time.Second):
					ok(id, map[string]any{"slept": true})
				case <-done:
					fail(id, "cancelled", "Request was cancelled")
				}
			}(req.ID)
		case "cancel":
			var p struct{ Target string }
			_ = json.Unmarshal(req.Params, &p)
			mu.Lock()
			ch, found := cancels[p.Target]
			delete(cancels, p.Target)
			mu.Unlock()
			if found {
				close(ch)
				// Record that cancellation reached the engine.
				fmt.Fprintln(os.Stderr, "[fake-engine] cancelled "+p.Target)
			}
			ok(req.ID, map[string]any{"cancelled": found})
		case "shutdown":
			if mode == "ignore-shutdown" {
				continue
			}
			ok(req.ID, map[string]any{})
			os.Exit(0)
		default:
			fail(req.ID, "method_not_found", "Unknown method: "+req.Method)
		}
	}
	if mode == "ignore-shutdown" {
		select {} // also ignore end of input
	}
	os.Exit(0)
}

// runFakeHost starts a supervisor whose engine never exits on its own, prints
// the engine's pid, and then blocks until it is killed.
func runFakeHost() {
	self, _ := os.Executable()
	os.Setenv("SONORA_FAKE_ENGINE", "ignore-shutdown")
	sup := New(Config{Command: self, Args: []string{"-test.run=^$"}})
	if err := sup.Start(context.Background()); err != nil {
		fmt.Println("error", err)
		os.Exit(1)
	}
	sup.mu.Lock()
	pid := sup.proc.cmd.Process.Pid
	sup.mu.Unlock()
	fmt.Println(pid)
	select {}
}

// newFake builds a supervisor that runs the fake engine in the given mode.
func newFake(t *testing.T, mode string, cfg Config) (*Supervisor, *statusLog) {
	t.Helper()
	self, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	t.Setenv("SONORA_FAKE_ENGINE", mode)
	// Under -race the fake engine (this binary) would otherwise sleep 1 s on exit.
	t.Setenv("GORACE", "atexit_sleep_ms=0")
	log := &statusLog{}
	cfg.Command = self
	cfg.Args = []string{"-test.run=^$"}
	cfg.OnStatus = log.add
	userLog := cfg.Log
	cfg.Log = func(line string) {
		log.addLine(line)
		if userLog != nil {
			userLog(line)
		}
	}
	sup := New(cfg)
	t.Cleanup(func() { _ = sup.Stop() })
	return sup, log
}

type statusLog struct {
	mu       sync.Mutex
	statuses []Status
	lines    []string
}

func (l *statusLog) add(s Status) {
	l.mu.Lock()
	l.statuses = append(l.statuses, s)
	l.mu.Unlock()
}

func (l *statusLog) addLine(s string) {
	l.mu.Lock()
	l.lines = append(l.lines, s)
	l.mu.Unlock()
}

func (l *statusLog) states() []State {
	l.mu.Lock()
	defer l.mu.Unlock()
	var out []State
	for _, s := range l.statuses {
		out = append(out, s.State)
	}
	return out
}

func (l *statusLog) hasLine(substr string) bool {
	l.mu.Lock()
	defer l.mu.Unlock()
	for _, line := range l.lines {
		if strings.Contains(line, substr) {
			return true
		}
	}
	return false
}

func waitFor(t *testing.T, what string, cond func() bool) {
	t.Helper()
	deadline := time.Now().Add(5 * time.Second)
	for time.Now().Before(deadline) {
		if cond() {
			return
		}
		time.Sleep(10 * time.Millisecond)
	}
	t.Fatalf("timed out waiting for %s", what)
}

func TestHandshakeAndCalls(t *testing.T) {
	sup, log := newFake(t, "normal", Config{})
	if _, err := sup.Call(context.Background(), "ping", nil); !errors.Is(err, ErrUnavailable) {
		t.Fatalf("call before start: got %v, want ErrUnavailable", err)
	}
	if err := sup.Start(context.Background()); err != nil {
		t.Fatal(err)
	}
	st := sup.Status()
	if st.State != StateReady || st.ServiceVersion != "0.0.0-fake" || st.Protocol != 1 || st.Starts != 1 {
		t.Fatalf("unexpected status %+v", st)
	}
	if got := log.states(); len(got) != 2 || got[0] != StateStarting || got[1] != StateReady {
		t.Fatalf("status sequence %v", got)
	}

	res, err := sup.Call(context.Background(), "echo", json.RawMessage(`{"n":[1,2,3],"s":"é"}`))
	if err != nil || string(res) != `{"n":[1,2,3],"s":"é"}` {
		t.Fatalf("echo: %s %v", res, err)
	}
	_, err = sup.Call(context.Background(), "fail", nil)
	var remote *RemoteError
	if !errors.As(err, &remote) || remote.Code != "invalid_params" {
		t.Fatalf("remote error not propagated: %v", err)
	}
	for _, m := range []string{"hello", "shutdown", "cancel"} {
		if _, err := sup.Call(context.Background(), m, nil); !errors.As(err, &remote) || remote.Code != "method_not_allowed" {
			t.Fatalf("reserved method %s was forwarded: %v", m, err)
		}
	}
	if !log.hasLine("[fake-engine] started") {
		t.Fatal("engine stderr was not forwarded to the log")
	}
}

func TestConcurrentCalls(t *testing.T) {
	sup, _ := newFake(t, "normal", Config{})
	if err := sup.Start(context.Background()); err != nil {
		t.Fatal(err)
	}
	var wg sync.WaitGroup
	errs := make(chan error, 200)
	for i := 0; i < 200; i++ {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			want := `{"i":` + strconv.Itoa(i) + `}`
			res, err := sup.Call(context.Background(), "echo", json.RawMessage(want))
			if err != nil || string(res) != want {
				errs <- fmt.Errorf("call %d: %s %v", i, res, err)
			}
		}(i)
	}
	wg.Wait()
	close(errs)
	for err := range errs {
		t.Error(err)
	}
}

func TestEventsAreForwarded(t *testing.T) {
	events := make(chan string, 1)
	sup, _ := newFake(t, "normal", Config{OnEvent: func(name string, data json.RawMessage) {
		events <- name + " " + string(data)
	}})
	if err := sup.Start(context.Background()); err != nil {
		t.Fatal(err)
	}
	if _, err := sup.Call(context.Background(), "emit", json.RawMessage(`{"pct":50}`)); err != nil {
		t.Fatal(err)
	}
	select {
	case got := <-events:
		if got != `progress {"pct":50}` {
			t.Fatalf("event %q", got)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("no event")
	}
}

func TestCancellationReachesEngine(t *testing.T) {
	sup, log := newFake(t, "normal", Config{})
	if err := sup.Start(context.Background()); err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()
	start := time.Now()
	_, err := sup.Call(ctx, "sleep", nil)
	if !errors.Is(err, context.DeadlineExceeded) || time.Since(start) > time.Second {
		t.Fatalf("cancelled call returned %v after %v", err, time.Since(start))
	}
	waitFor(t, "engine to receive cancel", func() bool { return log.hasLine("[fake-engine] cancelled") })
	// The engine's late "cancelled" answer is dropped; the connection stays usable.
	if _, err := sup.Call(context.Background(), "ping", nil); err != nil {
		t.Fatal(err)
	}
}

func TestCrashIsReportedAndRestartRecovers(t *testing.T) {
	sup, _ := newFake(t, "crash-on-ping", Config{})
	if err := sup.Start(context.Background()); err != nil {
		t.Fatal(err)
	}
	if _, err := sup.Call(context.Background(), "ping", nil); !errors.Is(err, ErrUnavailable) {
		t.Fatalf("in-flight call during crash: %v", err)
	}
	waitFor(t, "crashed state", func() bool { return sup.Status().State == StateCrashed })
	st := sup.Status()
	if !strings.Contains(st.Detail, "exit status 3") || !strings.Contains(st.Detail, "crashing as requested") {
		t.Fatalf("crash detail %q", st.Detail)
	}
	if _, err := sup.Call(context.Background(), "echo", nil); !errors.Is(err, ErrUnavailable) {
		t.Fatalf("call after crash: %v", err)
	}
	if err := sup.Restart(context.Background()); err != nil {
		t.Fatal(err)
	}
	if st := sup.Status(); st.State != StateReady || st.Starts != 2 {
		t.Fatalf("after restart: %+v", st)
	}
}

func TestIncompatibleEngines(t *testing.T) {
	for _, mode := range []string{"incompatible", "wrong-service"} {
		t.Run(mode, func(t *testing.T) {
			sup, _ := newFake(t, mode, Config{})
			if err := sup.Start(context.Background()); err == nil {
				t.Fatal("start succeeded")
			}
			if st := sup.Status(); st.State != StateIncompatible {
				t.Fatalf("status %+v", st)
			}
			if _, err := sup.Call(context.Background(), "ping", nil); !errors.Is(err, ErrUnavailable) {
				t.Fatalf("call: %v", err)
			}
		})
	}
}

func TestHandshakeTimeout(t *testing.T) {
	sup, _ := newFake(t, "silent", Config{HandshakeTimeout: 200 * time.Millisecond})
	if err := sup.Start(context.Background()); err == nil {
		t.Fatal("start succeeded")
	}
	if st := sup.Status(); st.State != StateFailed || !strings.Contains(st.Detail, "in time") {
		t.Fatalf("status %+v", st)
	}
}

func TestMissingExecutable(t *testing.T) {
	sup := New(Config{Command: `C:\definitely\not\here\sonora-service.exe`})
	if err := sup.Start(context.Background()); err == nil {
		t.Fatal("start succeeded")
	}
	if st := sup.Status(); st.State != StateFailed || st.Detail == "" {
		t.Fatalf("status %+v", st)
	}
}

func TestStopEscalatesToKill(t *testing.T) {
	sup, _ := newFake(t, "ignore-shutdown", Config{ShutdownTimeout: 300 * time.Millisecond})
	if err := sup.Start(context.Background()); err != nil {
		t.Fatal(err)
	}
	sup.mu.Lock()
	p := sup.proc
	sup.mu.Unlock()
	start := time.Now()
	if err := sup.Stop(); err != nil {
		t.Fatal(err)
	}
	select {
	case <-p.exited:
	default:
		t.Fatal("engine still running after Stop")
	}
	if elapsed := time.Since(start); elapsed > 2*time.Second {
		t.Fatalf("stop took %v", elapsed)
	}
	if st := sup.Status(); st.State != StateStopped {
		t.Fatalf("status %+v", st)
	}
	if err := sup.Stop(); err != nil { // idempotent
		t.Fatal(err)
	}
}

func TestOrderlyStop(t *testing.T) {
	sup, _ := newFake(t, "normal", Config{})
	if err := sup.Start(context.Background()); err != nil {
		t.Fatal(err)
	}
	sup.mu.Lock()
	p := sup.proc
	sup.mu.Unlock()
	start := time.Now()
	_ = sup.Stop()
	<-p.exited
	if p.exitErr != nil || time.Since(start) > time.Second {
		t.Fatalf("orderly stop: err=%v after %v", p.exitErr, time.Since(start))
	}
}

// If the host dies without stopping the engine, Windows must kill the engine.
func TestEngineDoesNotOutliveKilledHost(t *testing.T) {
	self, _ := os.Executable()
	host := exec.Command(self, "-test.run=^$")
	host.Env = append(os.Environ(), "SONORA_FAKE_ENGINE=host")
	out, err := host.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	if err := host.Start(); err != nil {
		t.Fatal(err)
	}
	line, err := bufio.NewReader(out).ReadString('\n')
	if err != nil {
		_ = host.Process.Kill()
		t.Fatalf("reading engine pid: %v", err)
	}
	pid, err := strconv.Atoi(strings.TrimSpace(line))
	if err != nil {
		_ = host.Process.Kill()
		t.Fatalf("host said %q", line)
	}
	if !processAlive(pid) {
		t.Fatal("engine not running before host kill")
	}
	_ = host.Process.Kill()
	_ = host.Wait()
	waitFor(t, "engine to be terminated with its host", func() bool { return !processAlive(pid) })
}
