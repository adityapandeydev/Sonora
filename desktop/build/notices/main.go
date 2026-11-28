// Command notices writes THIRD_PARTY_NOTICES.txt: the licences of everything
// compiled into Sonora's executables and frontend bundle.
//
// It reads what is actually built, not a hand-kept list:
//   - Haskell packages linked into sonora-service, from cabal's build plan;
//     licences from GHC's documentation (packages shipped with GHC) or from the
//     Hackage source archives in cabal's package cache.
//   - Go modules compiled into the desktop host (`go version -m`), plus Go's
//     standard library; licences from the module cache.
//   - npm packages the frontend depends on at run time; licences from
//     node_modules.
//   - Native runtime pieces statically linked by GHC's MinGW toolchain.
//
// Any dependency without a licence text found is an error, so a release
// cannot ship with a gap.
//
//	go run ./build/notices -plan ../dist-newstyle/cache/plan.json -exe bin/Sonora.exe -frontend frontend -out bin/THIRD_PARTY_NOTICES.txt
package main

import (
	"archive/tar"
	"compress/gzip"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

type notice struct {
	group, name, version, text string
}

var licenceFile = regexp.MustCompile(`(?i)^(licen[cs]e|copying)(\.[a-z]+)?$`)

func main() {
	plan := flag.String("plan", "../dist-newstyle/cache/plan.json", "cabal build plan")
	exe := flag.String("exe", "bin/Sonora.exe", "built desktop host")
	frontend := flag.String("frontend", "frontend", "frontend directory")
	out := flag.String("out", "bin/THIRD_PARTY_NOTICES.txt", "file to write")
	unit := flag.String("unit", "sonora-0.1.0.0-inplace-sonora-service", "plan unit of the engine executable")
	flag.Parse()

	var all []notice
	var missing []string
	add := func(group string) func(name, version, text string, err error) {
		return func(name, version, text string, err error) {
			if err != nil || strings.TrimSpace(text) == "" {
				missing = append(missing, fmt.Sprintf("%s %s %s: %v", group, name, version, err))
				return
			}
			all = append(all, notice{group, name, version, strings.TrimSpace(text)})
		}
	}

	haskell(*plan, *unit, add("Haskell"))
	goLicences := golang(*exe, add("Go"))
	npm(*frontend, goLicences, add("JavaScript"))
	native(add("Native runtime"))

	if len(missing) > 0 {
		fmt.Fprintln(os.Stderr, "licence texts not found:")
		for _, m := range missing {
			fmt.Fprintln(os.Stderr, "  "+m)
		}
		os.Exit(1)
	}
	if err := write(*out, all); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	fmt.Printf("wrote %d notices to %s\n", len(all), *out)
}

func run(name string, args ...string) (string, error) {
	b, err := exec.Command(name, args...).Output()
	return strings.TrimSpace(string(b)), err
}

// ---- Haskell

type unit struct {
	ID         string   `json:"id"`
	Name       string   `json:"pkg-name"`
	Version    string   `json:"pkg-version"`
	Type       string   `json:"type"`
	Depends    []string `json:"depends"`
	Components map[string]struct {
		Depends []string `json:"depends"`
	} `json:"components"`
}

func haskell(planPath, root string, add func(string, string, string, error)) {
	raw, err := os.ReadFile(planPath)
	if err != nil {
		add("build plan", "", "", err)
		return
	}
	var plan struct {
		Units []unit `json:"install-plan"`
	}
	if err := json.Unmarshal(raw, &plan); err != nil {
		add("build plan", "", "", err)
		return
	}
	units := map[string]unit{}
	for _, u := range plan.Units {
		units[u.ID] = u
	}
	// What is linked: the executable's dependencies, then only the library
	// parts of each dependency (not their tests or tools).
	seen := map[string]bool{}
	var visit func(id string, top bool)
	visit = func(id string, top bool) {
		u, ok := units[id]
		if !ok || seen[id] {
			return
		}
		seen[id] = true
		deps := append([]string{}, u.Depends...)
		for name, c := range u.Components {
			if name == "lib" || (top && strings.HasPrefix(name, "exe:")) {
				deps = append(deps, c.Depends...)
			}
		}
		for _, d := range deps {
			visit(d, false)
		}
	}
	visit(root, true)
	if len(seen) == 0 {
		add("build plan unit", root, "", fmt.Errorf("not in the plan"))
		return
	}

	libdir, _ := run("ghc", "--print-libdir")
	docs, _ := filepath.Glob(filepath.Join(libdir, "doc", "*"))
	cache, _ := run("cabal", "path", "--remote-repo-cache")
	store := ""
	if js, err := run("cabal", "path", "--output-format=json"); err == nil {
		var paths struct {
			Compiler struct {
				StorePath string `json:"store-path"`
			} `json:"compiler"`
		}
		if json.Unmarshal([]byte(js), &paths) == nil {
			store = paths.Compiler.StorePath
		}
	}

	done := map[string]bool{}
	for id := range seen {
		u := units[id]
		key := u.Name + "-" + u.Version
		if u.Name == "sonora" || done[key] {
			continue
		}
		done[key] = true
		// GHC's runtime system carries the GHC licence, as base does.
		lookup := u
		if u.Name == "rts" {
			for _, v := range units {
				if v.Name == "base" {
					lookup = v
				}
			}
		}
		text, err := ghcDoc(docs, lookup.Name+"-"+lookup.Version)
		if err != nil {
			// Packages built by cabal keep their licence in the store.
			text, err = firstLicence(filepath.Join(store, u.ID, "share", "doc"))
		}
		if err != nil {
			text, err = fromArchive(filepath.Join(cache, "hackage.haskell.org", u.Name, u.Version, key+".tar.gz"))
		}
		add(u.Name, u.Version, text, err)
	}
}

func ghcDoc(docs []string, dir string) (string, error) {
	for _, d := range docs {
		entries, err := os.ReadDir(filepath.Join(d, dir))
		if err != nil {
			continue
		}
		for _, e := range entries {
			if licenceFile.MatchString(e.Name()) {
				b, err := os.ReadFile(filepath.Join(d, dir, e.Name()))
				return string(b), err
			}
		}
	}
	return "", fmt.Errorf("no licence in GHC's documentation")
}

func fromArchive(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer f.Close()
	gz, err := gzip.NewReader(f)
	if err != nil {
		return "", err
	}
	tr := tar.NewReader(gz)
	for {
		h, err := tr.Next()
		if err == io.EOF {
			return "", fmt.Errorf("no licence file in %s", filepath.Base(path))
		}
		if err != nil {
			return "", err
		}
		parts := strings.Split(strings.Trim(h.Name, "/"), "/")
		if len(parts) == 2 && licenceFile.MatchString(parts[1]) {
			b, err := io.ReadAll(tr)
			return string(b), err
		}
	}
}

// ---- Go

// golang returns the licences it found, by module path, for npm packages
// that are published from the same project.
func golang(exe string, add func(string, string, string, error)) map[string]string {
	found := map[string]string{}
	root, _ := run("go", "env", "GOROOT")
	b, err := os.ReadFile(filepath.Join(root, "LICENSE"))
	version, _ := run("go", "env", "GOVERSION")
	add("Go standard library", version, string(b), err)

	info, err := run("go", "version", "-m", exe)
	if err != nil {
		add("go version -m", exe, "", err)
		return found
	}
	cache, _ := run("go", "env", "GOMODCACHE")
	for _, line := range strings.Split(info, "\n") {
		f := strings.Fields(line)
		if len(f) < 3 || f[0] != "dep" {
			continue
		}
		dir := filepath.Join(cache, escapeModule(f[1])+"@"+f[2])
		text, err := firstLicence(dir)
		add(f[1], f[2], text, err)
		found[f[1]] = text
	}
	return found
}

// npm packages that ship without a licence file but are part of a Go
// module's project, under its licence.
var sameProject = map[string]string{
	"@wailsio/runtime": "github.com/wailsapp/wails/v3",
}

// escapeModule applies the module cache's case encoding: "A" becomes "!a".
func escapeModule(path string) string {
	var b strings.Builder
	for _, r := range path {
		if r >= 'A' && r <= 'Z' {
			b.WriteRune('!')
			b.WriteRune(r + ('a' - 'A'))
		} else {
			b.WriteRune(r)
		}
	}
	return b.String()
}

func firstLicence(dir string) (string, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return "", err
	}
	for _, e := range entries {
		if !e.IsDir() && licenceFile.MatchString(e.Name()) {
			b, err := os.ReadFile(filepath.Join(dir, e.Name()))
			return string(b), err
		}
	}
	return "", fmt.Errorf("no licence file in %s", dir)
}

// ---- JavaScript

func npm(dir string, goLicences map[string]string, add func(string, string, string, error)) {
	type pkg struct {
		Version      string            `json:"version"`
		Dependencies map[string]string `json:"dependencies"`
	}
	read := func(path string) (pkg, error) {
		var p pkg
		b, err := os.ReadFile(path)
		if err == nil {
			err = json.Unmarshal(b, &p)
		}
		return p, err
	}
	top, err := read(filepath.Join(dir, "package.json"))
	if err != nil {
		add("package.json", "", "", err)
		return
	}
	seen := map[string]bool{}
	var visit func(name string)
	visit = func(name string) {
		if seen[name] {
			return
		}
		seen[name] = true
		pd := filepath.Join(dir, "node_modules", filepath.FromSlash(name))
		p, err := read(filepath.Join(pd, "package.json"))
		if err != nil {
			add(name, "", "", err)
			return
		}
		text, err := firstLicence(pd)
		if err != nil && goLicences[sameProject[name]] != "" {
			text, err = goLicences[sameProject[name]], nil
		}
		add(name, p.Version, text, err)
		for d := range p.Dependencies {
			visit(d)
		}
	}
	for d := range top.Dependencies {
		visit(d)
	}
}

// ---- Native runtime

func native(add func(string, string, string, error)) {
	libdir, _ := run("ghc", "--print-libdir")
	licences := filepath.Join(libdir, "..", "mingw", "share", "licenses")
	for _, part := range []struct{ name, file string }{
		{"MinGW-w64 runtime", "crt/COPYING.MinGW-w64-runtime.txt"},
		{"winpthreads", "winpthreads/COPYING"},
		{"libffi", "libffi/LICENSE"},
		{"LLVM compiler runtime", "llvm/LICENSE"},
	} {
		b, err := os.ReadFile(filepath.Join(licences, filepath.FromSlash(part.file)))
		add(part.name, "", string(b), err)
	}
}

// ---- Output

func write(path string, all []notice) error {
	order := map[string]int{"Native runtime": 3, "JavaScript": 2}
	sort.Slice(all, func(i, j int) bool {
		gi, gj := all[i].group, all[j].group
		if order[gi] != order[gj] {
			return order[gi] < order[gj]
		}
		if gi != gj {
			return gi < gj
		}
		return strings.ToLower(all[i].name) < strings.ToLower(all[j].name)
	})
	var b strings.Builder
	b.WriteString("Sonora includes the following third-party software.\r\n")
	b.WriteString("Each is listed with its licence, as its authors require.\r\n\r\n")
	for _, n := range all {
		title := n.name
		if n.version != "" {
			title += " " + n.version
		}
		b.WriteString(strings.Repeat("=", 72) + "\r\n")
		b.WriteString(title + "  (" + n.group + ")\r\n")
		b.WriteString(strings.Repeat("=", 72) + "\r\n\r\n")
		b.WriteString(strings.ReplaceAll(strings.ReplaceAll(n.text, "\r\n", "\n"), "\n", "\r\n"))
		b.WriteString("\r\n\r\n")
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}
	return os.WriteFile(path, []byte(b.String()), 0o644)
}
