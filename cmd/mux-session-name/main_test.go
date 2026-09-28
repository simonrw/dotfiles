package main

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

func TestSessionNameCLI(t *testing.T) {
	binary := filepath.Join(t.TempDir(), "mux-session-name")
	if output, err := exec.Command("go", "build", "-o", binary, ".").CombinedOutput(); err != nil {
		t.Fatalf("build: %v\n%s", err, output)
	}

	cwd := filepath.Join(t.TempDir(), "simon", "dotfiles")
	if err := os.MkdirAll(cwd, 0o755); err != nil {
		t.Fatal(err)
	}

	for _, tt := range []struct {
		name    string
		backend string
		args    []string
		want    string
	}{
		{name: "default cwd", want: "simon/dotfiles"},
		{name: "tmux absolute", backend: "tmux", args: []string{cwd}, want: "simon/dotfiles"},
		{name: "herdr cwd", backend: "herdr", want: "dotfiles"},
		{name: "herdr absolute", backend: "herdr", args: []string{cwd}, want: "dotfiles"},
		{name: "relative path", args: []string{"../dotfiles/"}, want: "simon/dotfiles"},
		{name: "removed worktree tmux", args: []string{"../dotfiles-feature"}, want: "simon/dotfiles-feature"},
		{name: "removed worktree herdr", backend: "herdr", args: []string{"../dotfiles-feature"}, want: "dotfiles-feature"},
		{name: "tmux punctuation", args: []string{"../my.repo: name"}, want: "simon/my-repo- name"},
		{name: "herdr punctuation", backend: "herdr", args: []string{"../my.repo: name"}, want: "my.repo: name"},
		{name: "explicit tmux", backend: "herdr", args: []string{"--backend", "tmux"}, want: "simon/dotfiles"},
		{name: "explicit herdr", backend: "tmux", args: []string{"--backend", "herdr"}, want: "dotfiles"},
		{name: "shallow path", args: []string{"/dotfiles"}, want: "dotfiles"},
		{name: "root", args: []string{"/"}, want: "/"},
	} {
		t.Run(tt.name, func(t *testing.T) {
			cmd := exec.Command(binary, tt.args...)
			cmd.Dir = cwd
			cmd.Env = append(os.Environ(), "SESSION_BACKEND="+tt.backend)
			output, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("run: %v\n%s", err, output)
			}
			if string(output) != tt.want+"\n" {
				t.Errorf("got %q, want %q", output, tt.want+"\n")
			}
		})
	}

	for _, args := range [][]string{{"--backend", "unknown"}, {"one", "two"}} {
		t.Run("invalid arguments "+args[0], func(t *testing.T) {
			cmd := exec.Command(binary, args...)
			cmd.Env = append(os.Environ(), "SESSION_BACKEND=tmux")
			if output, err := cmd.CombinedOutput(); err == nil {
				t.Fatalf("expected failure, got %q", output)
			}
		})
	}
}
