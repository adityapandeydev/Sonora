//go:build !windows

package engine

import "os/exec"

// Only Windows is verified; other platforms run the engine without
// host-lifetime enforcement until they are supported.
func configureCommand(cmd *exec.Cmd) {}

func attachJob(cmd *exec.Cmd) (func(), error) { return func() {}, nil }
