// Command rex-copy-mode browses a frozen snapshot of a Rex terminal's
// scrollback with Vim motions and copies a selection to the clipboard.
package main

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"strings"

	tea "charm.land/bubbletea/v2"
	"github.com/charmbracelet/colorprofile"
)

func main() {
	rex := flag.String("rex", "rex", "path to the rex CLI")
	session := flag.String("session", "", "source session ID")
	block := flag.String("block", "", "source terminal block ID")
	flag.Parse()
	if *session == "" || *block == "" {
		flag.Usage()
		os.Exit(2)
	}

	buf, err := capture(*rex, *session, *block)
	m := newModel(buf, pbcopy)
	if err != nil {
		m.message = "Copy mode failed: " + err.Error()
	}

	// Rex renders truecolor; detection would otherwise downsample captured colors.
	program := tea.NewProgram(m, tea.WithColorProfile(colorprofile.TrueColor))
	if _, err := program.Run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

// capture reads the source terminal's screen and scrollback with styles,
// joining soft-wrapped rows into logical lines.
func capture(rex, session, block string) (*buffer, error) {
	request, err := json.Marshal(map[string]any{
		"session_id": session,
		"block_id":   block,
		"args":       map[string]any{"format": "vt", "trim": true, "unwrap": true},
	})
	if err != nil {
		return parseSnapshot(""), err
	}

	var stderr bytes.Buffer
	cmd := exec.Command(rex, "--autostart=false", "api", "call", "com.superlogical.terminal.format", string(request))
	cmd.Stderr = &stderr
	output, err := cmd.Output()
	if err != nil {
		return parseSnapshot(""), fmt.Errorf("%w: %s", err, strings.TrimSpace(stderr.String()))
	}

	var response struct {
		Content string `json:"content"`
	}
	if err := json.Unmarshal(output, &response); err != nil {
		return parseSnapshot(""), err
	}
	return parseSnapshot(response.Content), nil
}

func pbcopy(text string) error {
	var stderr bytes.Buffer
	cmd := exec.Command("pbcopy")
	cmd.Stdin = strings.NewReader(text)
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("%w: %s", err, strings.TrimSpace(stderr.String()))
	}
	return nil
}
