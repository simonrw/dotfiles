package zedconfig

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestMergeSourceScalarOverridesTarget(t *testing.T) {
	got := mustMerge(t, `{"theme": "old"}`, `{"theme": "new"}`)

	assertJSONValue(t, got, "theme", "new")
}

func TestMergeNestedObjectPreservesTargetOnlyKeys(t *testing.T) {
	got := mustMerge(t, `{
  "agent": {
    "dock": "right",
    "version": 1
  }
}`, `{
  "agent": {
    "dock": "left",
    "thinking_display": "auto"
  }
}`)

	assertJSONValue(t, got, "agent.dock", "left")
	assertJSONValue(t, got, "agent.version", float64(1))
	assertJSONValue(t, got, "agent.thinking_display", "auto")
}

func TestMergeArraysReplaceTargetArrays(t *testing.T) {
	got := mustMerge(t, `{"wrap_guides": [80]}`, `{"wrap_guides": [100]}`)

	assertJSONValue(t, got, "wrap_guides", []any{float64(100)})
}

func TestMergeAcceptsJSONC(t *testing.T) {
	got := mustMerge(t, `{
  // Editor-managed value.
  "ssh_connections": [
    "work",
  ],
}`, `{
  "buffer_font_features": {
    /* Zed accepts comments here. */
    "calt": false,
  },
}`)

	assertJSONValue(t, got, "ssh_connections", []any{"work"})
	assertJSONValue(t, got, "buffer_font_features.calt", false)
}

func TestMergeReturnsErrorForInvalidJSONC(t *testing.T) {
	_, err := Merge([]byte(`{"theme": "old"}`), []byte(`{"theme":`))
	if err == nil {
		t.Fatal("expected invalid source JSONC to return an error")
	}

	if !strings.Contains(err.Error(), "parse source JSONC") {
		t.Fatalf("expected source parse context, got %q", err)
	}
}

func TestMergeFilesTreatsMissingTargetAsEmpty(t *testing.T) {
	dir := t.TempDir()
	source := writeTestFile(t, dir, "base.json", `{"theme": "new"}`)

	got, err := MergeFiles(filepath.Join(dir, "missing-settings.json"), source)
	if err != nil {
		t.Fatalf("merge with missing target: %v", err)
	}

	assertJSONValue(t, got, "theme", "new")
}

func TestMergeFilesReturnsErrorForMissingSource(t *testing.T) {
	dir := t.TempDir()
	target := writeTestFile(t, dir, "settings.json", `{"theme": "old"}`)

	_, err := MergeFiles(target, filepath.Join(dir, "missing-source.json"))
	if err == nil {
		t.Fatal("expected missing source file to return an error")
	}
	if !strings.Contains(err.Error(), "read source") {
		t.Fatalf("expected source read context, got %q", err)
	}
}

func mustMerge(t *testing.T, target, source string) []byte {
	t.Helper()

	got, err := Merge([]byte(target), []byte(source))
	if err != nil {
		t.Fatalf("merge JSONC: %v", err)
	}

	return got
}

func assertJSONValue(t *testing.T, data []byte, dottedKey string, want any) {
	t.Helper()

	var decoded map[string]any
	if err := json.Unmarshal(data, &decoded); err != nil {
		t.Fatalf("unmarshal merged JSON: %v", err)
	}

	parts := strings.Split(dottedKey, ".")
	var got any = decoded
	for _, part := range parts {
		values, ok := got.(map[string]any)
		if !ok {
			t.Fatalf("expected %q to be in an object, got %#v", part, got)
		}
		got = values[part]
	}

	if !deepEqualJSONValue(got, want) {
		t.Fatalf("expected %s = %#v, got %#v", dottedKey, want, got)
	}
}

func deepEqualJSONValue(got, want any) bool {
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
