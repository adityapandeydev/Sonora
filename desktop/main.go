// Command desktop is the Sonora desktop host: a Wails window around the
// React frontend, plus supervision of the Haskell engine process. All
// musical and project behavior lives in the engine.
package main

import (
	"embed"
	"encoding/json"
	"log"
	"os"
	"path/filepath"

	"github.com/adityapandeydev/Sonora/desktop/engine"
	"github.com/wailsapp/wails/v3/pkg/application"
)

//go:embed all:frontend/dist
var assets embed.FS

func init() {
	application.RegisterEvent[engine.Status](EventEngineStatus)
	application.RegisterEvent[EngineEvent](EventEngineEvent)
}

// enginePath locates sonora-service: SONORA_SERVICE_EXE if set, otherwise
// next to this executable.
func enginePath() string {
	if p := os.Getenv("SONORA_SERVICE_EXE"); p != "" {
		return p
	}
	exe, err := os.Executable()
	if err != nil {
		return "sonora-service.exe"
	}
	return filepath.Join(filepath.Dir(exe), "sonora-service.exe")
}

func main() {
	if path := setupLogging(); path != "" {
		log.Println("logging to", path)
	}
	var app *application.App
	sup := engine.New(engine.Config{
		Command: enginePath(),
		OnStatus: func(st engine.Status) {
			log.Printf("engine status: %s %s", st.State, st.Detail)
			if app != nil {
				app.Event.Emit(EventEngineStatus, st)
			}
		},
		OnEvent: func(name string, data json.RawMessage) {
			if app != nil {
				app.Event.Emit(EventEngineEvent, EngineEvent{Name: name, Data: data})
			}
		},
		Log: func(line string) { log.Println("[engine]", line) },
	})

	app = application.New(application.Options{
		Name:        "Sonora",
		Description: "Music composition and synthesis",
		Services: []application.Service{
			application.NewService(&EngineService{sup: sup}),
		},
		Assets: application.AssetOptions{
			Handler: application.AssetFileServerFS(assets),
		},
	})

	app.Window.NewWithOptions(application.WebviewWindowOptions{
		Title:     "Sonora",
		Width:     1280,
		Height:    800,
		MinWidth:  960,
		MinHeight: 600,
		URL:       "/",
	})

	if err := app.Run(); err != nil {
		log.Fatal(err)
	}
}
