package engine

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"strings"
	"sync"
	"testing"
	"time"
)

// TestRealService exercises the Haskell sonora-service. Set
// SONORA_SERVICE_EXE to its path (e.g. from `cabal list-bin sonora-service`).
func TestRealService(t *testing.T) {
	exe := os.Getenv("SONORA_SERVICE_EXE")
	if exe == "" {
		t.Skip("SONORA_SERVICE_EXE not set")
	}
	var mu sync.Mutex
	var lines []string
	events := make(chan string, 1)
	sup := New(Config{
		Command: exe,
		Args:    []string{"--enable-diagnostic-methods"},
		Log: func(l string) {
			mu.Lock()
			lines = append(lines, l)
			mu.Unlock()
		},
		OnEvent: func(name string, data json.RawMessage) { events <- name + " " + string(data) },
	})
	t.Cleanup(func() { _ = sup.Stop() })

	start := time.Now()
	if err := sup.Start(context.Background()); err != nil {
		t.Fatal(err)
	}
	t.Logf("engine ready after %v: %+v", time.Since(start), sup.Status())
	if st := sup.Status(); st.State != StateReady || st.Protocol != 1 || st.ServiceVersion == "" {
		t.Fatalf("status %+v", st)
	}

	// Round-trip latency over the private pipes.
	const n = 200
	t0 := time.Now()
	for i := 0; i < n; i++ {
		if _, err := sup.Call(context.Background(), "ping", nil); err != nil {
			t.Fatal(err)
		}
	}
	t.Logf("ping round trip: %v average over %d calls", time.Since(t0)/n, n)

	res, err := sup.Call(context.Background(), "diagnostic.echo", json.RawMessage(`{"text":"héllo","n":3}`))
	if err != nil || !strings.Contains(string(res), `"héllo"`) {
		t.Fatalf("echo: %s %v", res, err)
	}

	if _, err := sup.Call(context.Background(), "diagnostic.event", json.RawMessage(`{"k":1}`)); err != nil {
		t.Fatal(err)
	}
	select {
	case got := <-events:
		if got != `diagnostic {"k":1}` {
			t.Fatalf("event %q", got)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("no event from engine")
	}

	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()
	if _, err := sup.Call(ctx, "diagnostic.sleep", json.RawMessage(`{"ms":5000}`)); !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("cancelled sleep: %v", err)
	}
	if _, err := sup.Call(context.Background(), "ping", nil); err != nil {
		t.Fatalf("engine unusable after cancel: %v", err)
	}

	_, err = sup.Call(context.Background(), "no.such.method", nil)
	var remote *RemoteError
	if !errors.As(err, &remote) || remote.Code != "method_not_found" {
		t.Fatalf("unknown method: %v", err)
	}

	sup.mu.Lock()
	p := sup.proc
	sup.mu.Unlock()
	t1 := time.Now()
	_ = sup.Stop()
	<-p.exited
	t.Logf("orderly stop took %v", time.Since(t1))
	if p.exitErr != nil {
		t.Fatalf("engine exit: %v", p.exitErr)
	}
	mu.Lock()
	defer mu.Unlock()
	if !strings.Contains(strings.Join(lines, "\n"), "handshake complete") {
		t.Fatalf("engine log not forwarded: %v", lines)
	}
}
