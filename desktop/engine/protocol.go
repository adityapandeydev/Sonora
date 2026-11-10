// Package engine supervises the Sonora engine process (sonora-service) and
// forwards requests, responses, and events over its line-delimited JSON
// protocol. It holds no musical or application logic: request parameters and
// results pass through as opaque JSON.
package engine

import (
	"encoding/json"
	"errors"
	"fmt"
)

// ProtocolVersion is the engine protocol version this host speaks.
const ProtocolVersion = 1

// ServiceName is the name the engine reports during the handshake.
const ServiceName = "sonora-service"

// RemoteError is a structured error returned by the engine.
type RemoteError struct {
	Code    string          `json:"code"`
	Message string          `json:"message"`
	Data    json.RawMessage `json:"data,omitempty"`
}

func (e *RemoteError) Error() string {
	return fmt.Sprintf("engine error %s: %s", e.Code, e.Message)
}

// ErrUnavailable is returned for calls made while the engine is not ready,
// or that were in flight when the engine stopped or crashed.
var ErrUnavailable = errors.New("engine is not available")

type outgoing struct {
	V      int             `json:"v"`
	ID     string          `json:"id"`
	Method string          `json:"method"`
	Params json.RawMessage `json:"params"`
}

// incoming covers responses ({v,id,ok,result|error}) and events ({v,event,data}).
type incoming struct {
	V      int             `json:"v"`
	ID     *string         `json:"id"`
	OK     *bool           `json:"ok"`
	Result json.RawMessage `json:"result"`
	Error  *RemoteError    `json:"error"`
	Event  *string         `json:"event"`
	Data   json.RawMessage `json:"data"`
}

type helloResult struct {
	Protocol       int      `json:"protocol"`
	Service        string   `json:"service"`
	ServiceVersion string   `json:"serviceVersion"`
	Methods        []string `json:"methods"`
}
