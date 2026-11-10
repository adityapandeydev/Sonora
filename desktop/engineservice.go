package main

import (
	"context"
	"encoding/json"
	"errors"

	"github.com/adityapandeydev/Sonora/desktop/engine"
	"github.com/wailsapp/wails/v3/pkg/application"
)

// Events emitted to the frontend.
const (
	EventEngineStatus = "engine:status"
	EventEngineEvent  = "engine:event"
)

// EngineEvent is an engine event forwarded unchanged to the frontend.
type EngineEvent struct {
	Name string          `json:"name"`
	Data json.RawMessage `json:"data"`
}

// Reply is the outcome of an engine call. Exactly one of Result (when OK)
// and Error is set. Error codes are the engine's, plus "engine_unavailable"
// when the engine is not running.
type Reply struct {
	OK     bool                `json:"ok"`
	Result json.RawMessage     `json:"result,omitempty"`
	Error  *engine.RemoteError `json:"error,omitempty"`
}

// EngineService exposes the engine to the frontend. It forwards calls and
// events without interpreting them.
type EngineService struct {
	sup *engine.Supervisor
}

func (s *EngineService) ServiceStartup(ctx context.Context, _ application.ServiceOptions) error {
	// Start in the background so the window appears immediately and can
	// show the engine starting.
	go func() { _ = s.sup.Start(context.Background()) }()
	return nil
}

func (s *EngineService) ServiceShutdown() error {
	return s.sup.Stop()
}

// Status returns the current engine status.
func (s *EngineService) Status() engine.Status {
	return s.sup.Status()
}

// Restart stops any running engine and starts a fresh one.
func (s *EngineService) Restart(ctx context.Context) engine.Status {
	_ = s.sup.Restart(ctx)
	return s.sup.Status()
}

// Call sends a request to the engine. Cancelling the returned promise in the
// frontend cancels the request in the engine.
func (s *EngineService) Call(ctx context.Context, method string, params json.RawMessage) (Reply, error) {
	result, err := s.sup.Call(ctx, method, params)
	var remote *engine.RemoteError
	switch {
	case err == nil:
		return Reply{OK: true, Result: result}, nil
	case errors.As(err, &remote):
		return Reply{Error: remote}, nil
	case errors.Is(err, engine.ErrUnavailable):
		return Reply{Error: &engine.RemoteError{Code: "engine_unavailable", Message: "The engine is not running"}}, nil
	default:
		return Reply{}, err // cancellation
	}
}
