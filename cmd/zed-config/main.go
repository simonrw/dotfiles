package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/simonrw/dotfiles/internal/zedconfig"
)

func main() {
	if err := run(); err != nil {
		fmt.Fprintf(os.Stderr, "zed-config: %v\n", err)
		os.Exit(1)
	}
}

func run() error {
	defaultSource, err := defaultSourcePath()
	if err != nil {
		return err
	}

	sourcePath := flag.String("source", defaultSource, "path to the git-tracked Zed base settings")
	targetPath := flag.String("target", defaultTargetPath(), "path to the live Zed settings")
	dryRun := flag.Bool("dry-run", false, "print merged JSON instead of writing the target")
	flag.Parse()

	merged, err := zedconfig.MergeFiles(*targetPath, *sourcePath)
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

	return filepath.Join(wd, ".config", "zed", "base.json"), nil
}

func defaultTargetPath() string {
	if zedConfigDir := os.Getenv("ZED_CONFIG_DIR"); zedConfigDir != "" {
		return filepath.Join(zedConfigDir, "settings.json")
	}

	home, err := os.UserHomeDir()
	if err != nil {
		return filepath.Join(".config", "zed", "settings.json")
	}

	return filepath.Join(home, ".config", "zed", "settings.json")
}

func writeFilePreservingMode(path string, data []byte) error {
	info, err := os.Stat(path)
	perm := os.FileMode(0o600)
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("stat target %q: %w", path, err)
	}
	if err == nil {
		perm = info.Mode().Perm()
	}

	dir := filepath.Dir(path)
	temp, err := os.CreateTemp(dir, ".settings.json.*")
	if err != nil {
		return fmt.Errorf("create temporary file in %q: %w", dir, err)
	}

	tempPath := temp.Name()
	defer os.Remove(tempPath)

	if _, err := temp.Write(data); err != nil {
		temp.Close()
		return fmt.Errorf("write temporary file %q: %w", tempPath, err)
	}

	if err := temp.Chmod(perm); err != nil {
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
