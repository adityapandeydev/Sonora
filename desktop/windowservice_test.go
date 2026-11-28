package main

import (
	"testing"

	"github.com/adityapandeydev/Sonora/desktop/engine"
)

// Without a running engine nothing can be saved, so closing never waits.
func TestCloseIsNotHeldWithoutEngine(t *testing.T) {
	s := &WindowService{sup: engine.New(engine.Config{Command: "missing-engine.exe"})}
	if !s.mayClose() {
		t.Fatal("closing was held although the engine is not running")
	}
}

// Once the frontend says to close, the window closes.
func TestCloseAfterAnswer(t *testing.T) {
	s := &WindowService{sup: engine.New(engine.Config{Command: "missing-engine.exe"})}
	s.Close() // no window yet: must not panic
	if !s.allowed || !s.mayClose() {
		t.Fatal("Close did not allow closing")
	}
}

// A cancelled close resets the request, so the next close asks again.
func TestCancelResetsRequest(t *testing.T) {
	s := &WindowService{}
	s.pending, s.seen = true, true
	s.CloseCancelled()
	if s.pending || s.seen {
		t.Fatal("CloseCancelled left a request pending")
	}
	s.pending = true
	s.CloseRequestSeen()
	if !s.seen {
		t.Fatal("CloseRequestSeen was not recorded")
	}
}
