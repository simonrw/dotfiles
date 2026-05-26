package codexconfig

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/pelletier/go-toml/v2"
)

func TestMergeSourceScalarOverridesTarget(t *testing.T) {
	got := mustMerge(t, `model = "gpt-5"`, `model = "gpt-5.5"`)

	assertTOMLValue(t, got, "model", "gpt-5.5")
}

func TestMergeNestedTablePreservesTargetOnlyKeys(t *testing.T) {
	got := mustMerge(t, `
[features]
codex_hooks = false
js_repl = false
`, `
[features]
codex_hooks = true
remote_connections = true
`)

	assertTOMLValue(t, got, "features.codex_hooks", true)
	assertTOMLValue(t, got, "features.js_repl", false)
	assertTOMLValue(t, got, "features.remote_connections", true)
}

func TestMergeArraysReplaceTargetArrays(t *testing.T) {
	got := mustMerge(t, `notify = ["old", "value"]`, `notify = ["new"]`)

	assertTOMLValue(t, got, "notify", []any{"new"})
}

func TestMergeArraysOfTablesReplaceTargetArraysOfTables(t *testing.T) {
	got := mustMerge(t, `
[[hooks.SessionStart]]
matcher = "old"
`, `
[[hooks.SessionStart]]
matcher = "startup|resume|clear"

[[hooks.SessionStart.hooks]]
type = "command"
command = "csm-reporter"
timeout = 5
`)

	var decoded map[string]any
	if err := toml.Unmarshal(got, &decoded); err != nil {
		t.Fatalf("unmarshal merged TOML: %v", err)
	}

	hooks := decoded["hooks"].(map[string]any)
	sessionStart := hooks["SessionStart"].([]any)
	if len(sessionStart) != 1 {
		t.Fatalf("expected one SessionStart hook, got %d", len(sessionStart))
	}

	entry := sessionStart[0].(map[string]any)
	if entry["matcher"] != "startup|resume|clear" {
		t.Fatalf("expected source matcher, got %#v", entry["matcher"])
	}

	nestedHooks := entry["hooks"].([]any)
	nestedEntry := nestedHooks[0].(map[string]any)
	if nestedEntry["command"] != "csm-reporter" {
		t.Fatalf("expected source nested hook command, got %#v", nestedEntry["command"])
	}
}

func TestMergeReturnsErrorForInvalidTOML(t *testing.T) {
	_, err := Merge([]byte(`model = "gpt-5"`), []byte(`model =`))
	if err == nil {
		t.Fatal("expected invalid source TOML to return an error")
	}

	if !strings.Contains(err.Error(), "parse source TOML") {
		t.Fatalf("expected source parse context, got %q", err)
	}
}

func TestMergeFilesReturnsErrorForMissingFiles(t *testing.T) {
	dir := t.TempDir()
	target := filepath.Join(dir, "config.toml")

	_, err := MergeFiles(target, filepath.Join(dir, "missing-source.toml"))
	if err == nil {
		t.Fatal("expected missing source file to return an error")
	}
	if !strings.Contains(err.Error(), "read source") {
		t.Fatalf("expected source read context, got %q", err)
	}

	_, err = MergeFiles(filepath.Join(dir, "missing-target.toml"), writeTestFile(t, dir, "base.toml", `model = "gpt-5.5"`))
	if err == nil {
		t.Fatal("expected missing target file to return an error")
	}
	if !strings.Contains(err.Error(), "read target") {
		t.Fatalf("expected target read context, got %q", err)
	}
}

func mustMerge(t *testing.T, target, source string) []byte {
	t.Helper()

	got, err := Merge([]byte(target), []byte(source))
	if err != nil {
		t.Fatalf("merge TOML: %v", err)
	}

	return got
}

func assertTOMLValue(t *testing.T, data []byte, dottedKey string, want any) {
	t.Helper()

	var decoded map[string]any
	if err := toml.Unmarshal(data, &decoded); err != nil {
		t.Fatalf("unmarshal merged TOML: %v", err)
	}

	parts := strings.Split(dottedKey, ".")
	var got any = decoded
	for _, part := range parts {
		values, ok := got.(map[string]any)
		if !ok {
			t.Fatalf("expected %q to be in a table, got %#v", part, got)
		}
		got = values[part]
	}

	if !deepEqualTOMLValue(got, want) {
		t.Fatalf("expected %s = %#v, got %#v", dottedKey, want, got)
	}
}

func deepEqualTOMLValue(got, want any) bool {
	gotSlice, gotOK := got.([]any)
	wantSlice, wantOK := want.([]any)
	if gotOK && wantOK {
		if len(gotSlice) != len(wantSlice) {
			return false
		}
		for i := range gotSlice {
			if gotSlice[i] != wantSlice[i] {
				return false
			}
		}
		return true
	}

	return got == want
}

func writeTestFile(t *testing.T, dir, name, contents string) string {
	t.Helper()

	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte(contents), 0o600); err != nil {
		t.Fatalf("write test file: %v", err)
	}

	return path
}
