package main

import (
	"errors"
	"strings"
	"testing"

	tea "charm.land/bubbletea/v2"
)

const sample = "\x1b[0m\x1b[38;5;1mred\x1b[0m plain café 界 e\u0301\r\n" +
	"foo.bar(baz) qux_1\r\n" +
	"\r\n" +
	"  second paragraph\r\n" +
	"more text\r\n" +
	"\r\n" +
	"$ last\r\n" +
	"\r\n"

func TestParseSnapshot(t *testing.T) {
	buf := parseSnapshot(sample)

	if len(buf.lines) != 7 {
		t.Fatalf("got %d lines, want trailing blank lines dropped", len(buf.lines))
	}

	first := buf.lines[0]
	if first[0].style != "\x1b[38;5;1m" || first[3].style != "" {
		t.Errorf("styles not tracked: %q %q", first[0].style, first[3].style)
	}
	if last := first[15]; last.text != "界" || last.width != 2 {
		t.Errorf("wide cell: %+v", last)
	}
	if mark := first[len(first)-1]; mark.text != "e\u0301" || mark.width != 1 {
		t.Errorf("combining mark not joined: %+v", mark)
	}

	if got := buf.text(pos{0, 4}, pos{0, 8}, false); got != "plain" {
		t.Errorf("text = %q", got)
	}
	if got := buf.text(pos{1, 13}, pos{0, 13}, false); got != "é 界 e\u0301\nfoo.bar(baz) q" {
		t.Errorf("reversed multiline text = %q", got)
	}
	if got := buf.text(pos{4, 3}, pos{3, 0}, true); got != "  second paragraph\nmore text\n" {
		t.Errorf("linewise text = %q", got)
	}
}

func TestMotions(t *testing.T) {
	buf := parseSnapshot(sample)

	cases := []struct {
		name   string
		motion func(pos) pos
		from   pos
		want   pos
	}{
		{"w punctuation", func(p pos) pos { return buf.wordForward(p, false) }, pos{1, 0}, pos{1, 3}},
		{"w crosses line", func(p pos) pos { return buf.wordForward(p, false) }, pos{1, 13}, pos{2, 0}},
		{"w leaves empty line", func(p pos) pos { return buf.wordForward(p, false) }, pos{2, 0}, pos{3, 2}},
		{"W", func(p pos) pos { return buf.wordForward(p, true) }, pos{1, 0}, pos{1, 13}},
		{"e", func(p pos) pos { return buf.wordEnd(p, false) }, pos{1, 0}, pos{1, 2}},
		{"e skips empty lines", func(p pos) pos { return buf.wordEnd(p, false) }, pos{1, 17}, pos{3, 7}},
		{"E", func(p pos) pos { return buf.wordEnd(p, true) }, pos{1, 0}, pos{1, 11}},
		{"b", func(p pos) pos { return buf.wordBackward(p, false) }, pos{1, 8}, pos{1, 7}},
		{"b stops at empty line", func(p pos) pos { return buf.wordBackward(p, false) }, pos{3, 2}, pos{2, 0}},
		{"B", func(p pos) pos { return buf.wordBackward(p, true) }, pos{1, 13}, pos{1, 0}},
		{"}", buf.paragraphForward, pos{0, 3}, pos{2, 0}},
		{"} from blank", buf.paragraphForward, pos{2, 0}, pos{5, 0}},
		{"} at end", buf.paragraphForward, pos{6, 0}, pos{6, 5}},
		{"{", buf.paragraphBackward, pos{4, 3}, pos{2, 0}},
		{"{ at start", buf.paragraphBackward, pos{1, 3}, pos{0, 0}},
		{"^", func(p pos) pos { return buf.firstNonBlank(p.line) }, pos{3, 10}, pos{3, 2}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := tc.motion(tc.from); got != tc.want {
				t.Errorf("got %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestSearch(t *testing.T) {
	buf := parseSnapshot(sample)

	if got, _ := buf.search(pos{0, 0}, "TEXT", true); got != (pos{0, 0}) {
		t.Errorf("upper case query should match case-sensitively, got %+v", got)
	}
	if got, ok := buf.search(pos{0, 0}, "text", true); !ok || got != (pos{4, 5}) {
		t.Errorf("forward search = %+v", got)
	}
	if got, ok := buf.search(pos{0, 0}, "界", false); !ok || got != (pos{0, 15}) {
		t.Errorf("backward search should wrap, got %+v", got)
	}
	if got, ok := buf.search(pos{0, 15}, "界", true); !ok || got != (pos{0, 15}) {
		t.Errorf("forward search should wrap to the only match, got %+v", got)
	}
}

// session drives a model through key presses at a fixed terminal size.
type session struct {
	t      *testing.T
	m      *model
	copied []string
	quit   bool
}

func newSession(t *testing.T, content string, width, height int) *session {
	s := &session{t: t}
	s.m = newModel(parseSnapshot(content), func(text string) error {
		s.copied = append(s.copied, text)
		return nil
	})
	s.m.Update(tea.WindowSizeMsg{Width: width, Height: height})
	return s
}

func (s *session) keys(keys ...string) {
	named := map[string]tea.KeyPressMsg{
		"enter":     {Code: tea.KeyEnter},
		"esc":       {Code: tea.KeyEscape},
		"space":     {Code: tea.KeySpace, Text: " "},
		"backspace": {Code: tea.KeyBackspace},
		"ctrl+d":    {Code: 'd', Mod: tea.ModCtrl},
		"ctrl+u":    {Code: 'u', Mod: tea.ModCtrl},
	}
	for _, k := range keys {
		msg, ok := named[k]
		if !ok {
			msg = tea.KeyPressMsg{Code: []rune(k)[0], Text: k}
		}
		if msg.String() != k {
			s.t.Fatalf("key %q is reported as %q", k, msg.String())
		}

		_, cmd := s.m.Update(msg)
		if cmd != nil {
			_, s.quit = cmd().(tea.QuitMsg)
		}
	}
}

func TestCopySelections(t *testing.T) {
	cases := []struct {
		name string
		keys []string
		want string
	}{
		{"word to end of line", []string{"g", "g", "w", "space", "$", "enter"}, "plain café 界 e\u0301"},
		{"counted motions", []string{"2", "g", "g", "v", "3", "e", "y"}, "foo.bar"},
		{"paragraph", []string{"4", "G", "V", "}", "y"}, "  second paragraph\nmore text\n\n"},
		{"search then select", []string{"/", "p", "a", "r", "enter", "v", "e", "y"}, "paragraph"},
		{"swap ends", []string{"v", "$", "o", "h", "y"}, "$ last"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			s := newSession(t, sample, 40, 10)

			s.keys(tc.keys...)

			if !s.quit || len(s.copied) != 1 || s.copied[0] != tc.want {
				t.Errorf("quit=%v copied=%q, want %q", s.quit, s.copied, tc.want)
			}
		})
	}
}

func TestVerticalMotionFollowsWrappedRows(t *testing.T) {
	s := newSession(t, "short\r\n"+strings.Repeat("abcdefghij", 3)+"\r\nend", 10, 5)

	s.keys("g", "g", "l", "l", "j", "j")
	if s.m.cursor != (pos{1, 12}) {
		t.Errorf("cursor = %+v, want the second wrapped row", s.m.cursor)
	}

	s.keys("$", "k")
	if s.m.cursor != (pos{1, 19}) {
		t.Errorf("after $ k cursor = %+v, want end of the previous wrapped row", s.m.cursor)
	}
}

func TestStartsAtBottomOfSnapshot(t *testing.T) {
	var lines []string
	for i := range 50 {
		lines = append(lines, strings.Repeat("x", i%3))
	}
	s := newSession(t, strings.Join(lines, "\r\n"), 20, 11)

	if s.m.cursor != (pos{49, 0}) || s.m.top != 40 {
		t.Errorf("cursor=%+v top=%d", s.m.cursor, s.m.top)
	}

	s.keys("ctrl+u")
	if s.m.top != 35 || s.m.cursor.line != 44 {
		t.Errorf("after ctrl+u cursor=%+v top=%d", s.m.cursor, s.m.top)
	}
}

func TestEscapeAndEnter(t *testing.T) {
	s := newSession(t, sample, 40, 10)

	s.keys("enter")
	if s.quit {
		t.Fatal("enter without a selection should do nothing")
	}

	s.keys("v", "esc")
	if s.quit || s.m.anchor != nil {
		t.Fatal("escape should first clear the selection")
	}

	s.keys("esc")
	if !s.quit || len(s.copied) != 0 {
		t.Errorf("escape should quit without copying, copied=%q", s.copied)
	}
}

func TestClipboardFailureKeepsSelection(t *testing.T) {
	s := newSession(t, sample, 40, 10)
	s.m.copy = func(string) error { return errors.New("no pbcopy") }

	s.keys("v", "l", "y")

	if s.quit || s.m.anchor == nil {
		t.Fatal("viewer should stay open with the selection")
	}
	if !strings.Contains(s.m.View().Content, "Could not copy: no pbcopy") {
		t.Error("error is not shown")
	}
}

func TestResizeKeepsBottomAnchoredUntilMoved(t *testing.T) {
	var lines []string
	for range 50 {
		lines = append(lines, "x")
	}
	s := newSession(t, strings.Join(lines, "\r\n"), 20, 11)

	s.m.Update(tea.WindowSizeMsg{Width: 20, Height: 21})
	if s.m.top != 30 {
		t.Errorf("top = %d, want the last line at the bottom after resize", s.m.top)
	}

	s.keys("k")
	s.m.Update(tea.WindowSizeMsg{Width: 20, Height: 31})
	if s.m.top != 30 {
		t.Errorf("top = %d, want the view kept once the cursor moved", s.m.top)
	}
}
