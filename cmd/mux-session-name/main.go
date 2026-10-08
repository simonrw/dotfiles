package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	backend := os.Getenv("SESSION_BACKEND")
	if backend == "" {
		backend = "tmux"
	}
	flag.StringVar(&backend, "backend", backend, "session backend: tmux, herdr, or rex")
	flag.Usage = func() {
		fmt.Fprintln(os.Stderr, "Usage: mux-session-name [--backend tmux|herdr|rex] [path]")
		flag.PrintDefaults()
	}
	flag.Parse()
	if flag.NArg() > 1 {
		flag.Usage()
		os.Exit(2)
	}

	path := "."
	if flag.NArg() == 1 {
		path = flag.Arg(0)
	}
	path, err := filepath.Abs(path)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	name := filepath.Base(path)
	switch backend {
	case "tmux", "rex":
		parent := filepath.Base(filepath.Dir(path))
		if parent != string(filepath.Separator) {
			name = parent + "/" + name
		}
	case "herdr":
	default:
		fmt.Fprintf(os.Stderr, "unknown session backend: %s\n", backend)
		os.Exit(2)
	}
	name = strings.NewReplacer(".", "-", ":", "-", " ", "").Replace(name)
	fmt.Println(name)
}
