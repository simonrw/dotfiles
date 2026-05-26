package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/simonrw/dotfiles/internal/codexconfig"
)

func main() {
	if err := run(); err != nil {
		fmt.Fprintf(os.Stderr, "codex-config: %v\n", err)
		os.Exit(1)
	}
}

func run() error {
	defaultSource, err := defaultSourcePath()
	if err != nil {
		return err
	}

	sourcePath := flag.String("source", defaultSource, "path to the git-tracked Codex base config")
	targetPath := flag.String("target", defaultTargetPath(), "path to the live Codex config")
	dryRun := flag.Bool("dry-run", false, "print merged TOML instead of writing the target")
	flag.Parse()

	merged, err := codexconfig.MergeFiles(*targetPath, *sourcePath)
	if err != nil {
		return err
	}

	if *dryRun {
		_, err := os.Stdout.Write(merged)
		return err
	}

	return writeFilePreservingMode(*targetPath, merged)
}

func defaultSourcePath() (string, error) {
	wd, err := os.Getwd()
	if err != nil {
		return "", fmt.Errorf("get working directory: %w", err)
	}

	return filepath.Join(wd, ".codex", "base.toml"), nil
}

func defaultTargetPath() string {
	if codexHome := os.Getenv("CODEX_HOME"); codexHome != "" {
		return filepath.Join(codexHome, "config.toml")
	}

	home, err := os.UserHomeDir()
	if err != nil {
		return filepath.Join(".codex", "config.toml")
	}

	return filepath.Join(home, ".codex", "config.toml")
}

func writeFilePreservingMode(path string, data []byte) error {
	info, err := os.Stat(path)
	if err != nil {
		return fmt.Errorf("stat target %q: %w", path, err)
	}

	dir := filepath.Dir(path)
	temp, err := os.CreateTemp(dir, ".config.toml.*")
	if err != nil {
		return fmt.Errorf("create temporary file in %q: %w", dir, err)
	}

	tempPath := temp.Name()
	defer os.Remove(tempPath)

	if _, err := temp.Write(data); err != nil {
		temp.Close()
		return fmt.Errorf("write temporary file %q: %w", tempPath, err)
	}

	if err := temp.Chmod(info.Mode().Perm()); err != nil {
		temp.Close()
		return fmt.Errorf("chmod temporary file %q: %w", tempPath, err)
	}

	if err := temp.Close(); err != nil {
		return fmt.Errorf("close temporary file %q: %w", tempPath, err)
	}

	if err := os.Rename(tempPath, path); err != nil {
		return fmt.Errorf("replace target %q: %w", path, err)
	}

	return nil
}
