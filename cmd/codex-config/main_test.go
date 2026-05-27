package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestWriteFilePreservingModeCreatesMissingTarget(t *testing.T) {
	dir := t.TempDir()
	target := filepath.Join(dir, "config.toml")

	if err := writeFilePreservingMode(target, []byte("model = \"gpt-5.5\"\n")); err != nil {
		t.Fatalf("write missing target: %v", err)
	}

	got, err := os.ReadFile(target)
	if err != nil {
		t.Fatalf("read target: %v", err)
	}
	if string(got) != "model = \"gpt-5.5\"\n" {
		t.Fatalf("unexpected target contents: %q", got)
	}

	info, err := os.Stat(target)
	if err != nil {
		t.Fatalf("stat target: %v", err)
	}
	if info.Mode().Perm() != 0o600 {
		t.Fatalf("expected new target mode 0600, got %o", info.Mode().Perm())
	}
}

func TestWriteFilePreservingModePreservesExistingMode(t *testing.T) {
	dir := t.TempDir()
	target := filepath.Join(dir, "config.toml")
	if err := os.WriteFile(target, []byte("old = true\n"), 0o640); err != nil {
		t.Fatalf("write existing target: %v", err)
	}
	if err := os.Chmod(target, 0o640); err != nil {
		t.Fatalf("chmod existing target: %v", err)
	}

	if err := writeFilePreservingMode(target, []byte("model = \"gpt-5.5\"\n")); err != nil {
		t.Fatalf("replace target: %v", err)
	}

	info, err := os.Stat(target)
	if err != nil {
		t.Fatalf("stat target: %v", err)
	}
	if info.Mode().Perm() != 0o640 {
		t.Fatalf("expected preserved target mode 0640, got %o", info.Mode().Perm())
	}
}
