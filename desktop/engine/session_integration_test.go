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

type sessionView struct {
	Revision       int    `json:"revision"`
	ScoreText      string `json:"scoreText"`
	ScoreIsCurrent bool   `json:"scoreIsCurrent"`
	Diagnostics    []struct {
		Severity string `json:"severity"`
		Line     *int   `json:"line"`
		Message  string `json:"message"`
	} `json:"diagnostics"`
	Render struct {
		State  string `json:"state"`
		Latest *struct {
			JobID           int     `json:"jobId"`
			Revision        int     `json:"revision"`
			DurationSeconds float64 `json:"durationSeconds"`
			RenderMillis    int     `json:"renderMillis"`
			Current         bool    `json:"current"`
		} `json:"latest"`
	} `json:"render"`
}

type transportView struct {
	Playing         bool    `json:"playing"`
	PositionSeconds float64 `json:"positionSeconds"`
	DurationSeconds float64 `json:"durationSeconds"`
	Current         bool    `json:"current"`
	LoopActive      bool    `json:"loopActive"`
	Volume          float64 `json:"volume"`
}

type eventLog struct {
	mu     sync.Mutex
	events []string
}

func (l *eventLog) add(name string, data json.RawMessage) {
	l.mu.Lock()
	l.events = append(l.events, name+" "+string(data))
	l.mu.Unlock()
}

func (l *eventLog) count(prefix string) int {
	l.mu.Lock()
	defer l.mu.Unlock()
	n := 0
	for _, e := range l.events {
		if strings.HasPrefix(e, prefix) {
			n++
		}
	}
	return n
}

func call[T any](t *testing.T, sup *Supervisor, method string, params any) T {
	t.Helper()
	raw, err := json.Marshal(params)
	if err != nil {
		t.Fatal(err)
	}
	res, err := sup.Call(context.Background(), method, raw)
	if err != nil {
		t.Fatalf("%s: %v", method, err)
	}
	var out T
	if err := json.Unmarshal(res, &out); err != nil {
		t.Fatalf("%s: decoding %s: %v", method, res, err)
	}
	return out
}

// TestRealServiceSession drives the edit → render → play slice against the
// Haskell engine. Set SONORA_AUDIO_SMOKE=1 to also play on the real output
// device for a moment.
func TestRealServiceSession(t *testing.T) {
	exe := os.Getenv("SONORA_SERVICE_EXE")
	if exe == "" {
		t.Skip("SONORA_SERVICE_EXE not set")
	}
	log := &eventLog{}
	sup := New(Config{Command: exe, OnEvent: log.add})
	t.Cleanup(func() { _ = sup.Stop() })
	if err := sup.Start(context.Background()); err != nil {
		t.Fatal(err)
	}

	view := call[sessionView](t, sup, "project.get", map[string]any{})
	if view.Render.State != "idle" || !view.ScoreIsCurrent || !strings.Contains(view.ScoreText, "track") {
		t.Fatalf("initial project: %+v", view)
	}

	start := time.Now()
	call[json.RawMessage](t, sup, "render.start", map[string]any{})
	waitFor(t, "first render", func() bool { return log.count("render.finished") == 1 })
	view = call[sessionView](t, sup, "project.get", map[string]any{})
	if view.Render.State != "ready" || view.Render.Latest == nil || !view.Render.Latest.Current {
		t.Fatalf("after render: %+v", view.Render)
	}
	t.Logf("starter score rendered: %.1f s of audio in %d ms (%v including round trips)",
		view.Render.Latest.DurationSeconds, view.Render.Latest.RenderMillis, time.Since(start))

	broken := call[sessionView](t, sup, "project.setScore", map[string]any{"text": "track x (lead\n  C4 quarter\n"})
	if broken.ScoreIsCurrent || len(broken.Diagnostics) != 1 || broken.Diagnostics[0].Line == nil ||
		broken.Render.State != "ready" {
		t.Fatalf("invalid edit: %+v", broken)
	}

	edited := call[sessionView](t, sup, "project.setScore", map[string]any{"text": "track x (lead):\n  C4 quarter\n  E4 quarter\n"})
	if !edited.ScoreIsCurrent || edited.Render.State != "rendering" {
		t.Fatalf("valid edit: %+v", edited)
	}
	waitFor(t, "second render", func() bool { return log.count("render.finished") == 2 })
	view = call[sessionView](t, sup, "project.get", map[string]any{})
	if view.Render.Latest == nil || view.Render.Latest.Revision != edited.Revision || !view.Render.Latest.Current {
		t.Fatalf("second render not tagged with the edited revision: %+v (edit revision %d)", view.Render.Latest, edited.Revision)
	}

	peaks := call[struct {
		JobID int          `json:"jobId"`
		Peaks [][2]float64 `json:"peaks"`
	}](t, sup, "render.peaks", map[string]any{"buckets": 512})
	if len(peaks.Peaks) != 512 || peaks.JobID != view.Render.Latest.JobID {
		t.Fatalf("peaks: job %d, %d buckets", peaks.JobID, len(peaks.Peaks))
	}
	sought := call[transportView](t, sup, "transport.seek", map[string]any{"seconds": 0.5})
	looped := call[transportView](t, sup, "transport.setLoop", map[string]any{"loop": map[string]any{"start": 0.25, "end": 0.75}})
	quiet := call[transportView](t, sup, "transport.setVolume", map[string]any{"volume": 0})
	if sought.PositionSeconds != 0.5 || !looped.LoopActive || quiet.Volume != 0 {
		t.Fatalf("seek %+v, loop %+v, volume %+v", sought, looped, quiet)
	}
	for method, params := range map[string]string{
		"transport.seek":      `{"seconds":-1}`,
		"transport.setLoop":   `{"loop":{"start":1,"end":1}}`,
		"transport.setVolume": `{"volume":2}`,
		"render.peaks":        `{"buckets":0}`,
	} {
		var remote *RemoteError
		if _, err := sup.Call(context.Background(), method, json.RawMessage(params)); !errors.As(err, &remote) || remote.Code != "invalid_value" {
			t.Fatalf("%s %s: %v", method, params, err)
		}
	}
	call[transportView](t, sup, "transport.setLoop", map[string]any{"loop": nil})
	call[transportView](t, sup, "transport.stop", map[string]any{})

	if os.Getenv("SONORA_AUDIO_SMOKE") != "1" {
		return
	}
	call[transportView](t, sup, "project.setScore", map[string]any{"text": strings.Repeat("track x (pluck):\n  C4 quarter\n  E4 quarter\n  G4 half\n", 1)})
	waitFor(t, "playback render", func() bool { return log.count("render.finished") == 3 })
	played := call[transportView](t, sup, "transport.play", map[string]any{})
	if !played.Playing {
		t.Fatalf("play: %+v", played)
	}
	time.Sleep(1200 * time.Millisecond)
	mid := call[transportView](t, sup, "transport.state", map[string]any{})
	stopped := call[transportView](t, sup, "transport.stop", map[string]any{})
	t.Logf("device playback: position %.2f s after 1.2 s; %d transport events", mid.PositionSeconds, log.count("transport "))
	if !mid.Playing || mid.PositionSeconds < 0.8 || mid.PositionSeconds > 1.3 || stopped.Playing || stopped.PositionSeconds != 0 {
		t.Fatalf("transport: mid %+v, stopped %+v", mid, stopped)
	}
}
