package main

import (
	"sync"

	"github.com/adityapandeydev/Sonora/desktop/engine"
	"github.com/wailsapp/wails/v3/pkg/application"
	"github.com/wailsapp/wails/v3/pkg/events"
)

// EventCloseRequested asks the frontend whether the window may close.
const EventCloseRequested = "app:close-requested"

// WindowService lets the frontend decide whether closing the window would
// lose work. The frontend answers a close request by calling Close once it
// has saved or the user chose not to. It never blocks closing for good: when
// the engine is not running, or the page did not acknowledge the previous
// request, the window closes at once.
type WindowService struct {
	window *application.WebviewWindow
	sup    *engine.Supervisor
	emit   func(name string)

	mu      sync.Mutex
	allowed bool // the frontend said to close
	pending bool // a request was sent
	seen    bool // the frontend acknowledged it
}

// Guard installs the close hook on the window.
func (s *WindowService) Guard(w *application.WebviewWindow) {
	s.window = w
	w.RegisterHook(events.Common.WindowClosing, func(e *application.WindowEvent) {
		if s.mayClose() {
			return
		}
		e.Cancel()
		s.emit(EventCloseRequested)
	})
}

func (s *WindowService) mayClose() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	switch {
	case s.allowed:
		return true
	case s.sup.Status().State != engine.StateReady:
		return true // nothing to save through
	case s.pending && !s.seen:
		return true // the page is not answering
	}
	s.pending, s.seen = true, false
	return false
}

// CloseRequestSeen is called by the frontend when a close request arrives.
func (s *WindowService) CloseRequestSeen() {
	s.mu.Lock()
	s.seen = true
	s.mu.Unlock()
}

// CloseCancelled is called when the user chose to keep the window open.
func (s *WindowService) CloseCancelled() {
	s.mu.Lock()
	s.pending, s.seen = false, false
	s.mu.Unlock()
}

// Close closes the window; the frontend calls it once nothing will be lost.
func (s *WindowService) Close() {
	s.mu.Lock()
	s.allowed = true
	w := s.window
	s.mu.Unlock()
	if w != nil {
		w.Close()
	}
}
