package main

import (
	"io"
	"log"
	"os"
	"path/filepath"
)

// setupLogging sends host and engine diagnostics to
// %LOCALAPPDATA%\Sonora\logs\desktop.log (the previous run is kept as
// desktop.prev.log) as well as stderr, since release builds have no console.
// It returns the log path, or "" if the file could not be opened.
func setupLogging() string {
	base, err := os.UserCacheDir()
	if err != nil {
		return ""
	}
	dir := filepath.Join(base, "Sonora", "logs")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return ""
	}
	path := filepath.Join(dir, "desktop.log")
	_ = os.Rename(path, filepath.Join(dir, "desktop.prev.log"))
	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
	if err != nil {
		return ""
	}
	// GUI builds have no usable stderr; MultiWriter stops at the first
	// failing writer, so only include stderr when it is valid.
	var out io.Writer = f
	if _, err := os.Stderr.Stat(); err == nil {
		out = io.MultiWriter(f, os.Stderr)
	}
	log.SetOutput(out)
	log.SetFlags(log.LstdFlags | log.Lmicroseconds)
	return path
}
