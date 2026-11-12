package main

import (
	"sort"
	"strings"
	"unicode"

	"github.com/charmbracelet/x/ansi"
)

// cell is one grapheme cluster as the terminal drew it.
type cell struct {
	text  string
	width int
	// style holds the SGR sequences in effect, ready to re-emit verbatim.
	style string
}

type line []cell

// pos addresses a cell by logical line and cell index.
type pos struct {
	line, col int
}

func (p pos) before(o pos) bool {
	return p.line < o.line || (p.line == o.line && p.col < o.col)
}

// buffer is a frozen snapshot of terminal output, one entry per logical line.
type buffer struct {
	lines []line
}

// parseSnapshot reads Rex's unwrapped "vt" capture, which holds only text, SGR
// sequences and line breaks.
func parseSnapshot(vt string) *buffer {
	raw := strings.Split(strings.ReplaceAll(vt, "\r\n", "\n"), "\n")
	lines := make([]line, 0, len(raw))
	for _, text := range raw {
		lines = append(lines, parseLine(text))
	}

	for len(lines) > 1 && len(lines[len(lines)-1]) == 0 {
		lines = lines[:len(lines)-1]
	}
	return &buffer{lines: lines}
}

func parseLine(raw string) line {
	var (
		cells []cell
		style string
		state byte
	)
	for len(raw) > 0 {
		seq, width, n, next := ansi.DecodeSequence(raw, state, nil)
		state = next
		raw = raw[n:]

		switch {
		case width > 0:
			cells = append(cells, cell{text: seq, width: width, style: style})
		// The decoder splits a combining mark from a preceding ASCII letter.
		case len(cells) > 0 && unicode.IsPrint([]rune(seq)[0]):
			cells[len(cells)-1].text += seq
		case ansi.HasCsiPrefix(seq) && strings.HasSuffix(seq, "m"):
			if seq == "\x1b[0m" || seq == "\x1b[m" {
				style = ""
			} else {
				style += seq
			}
		}
	}
	return cells
}

func (b *buffer) lastCol(l int) int {
	return max(len(b.lines[l])-1, 0)
}

func (b *buffer) end() pos {
	last := len(b.lines) - 1
	return pos{last, b.lastCol(last)}
}

// next steps one cell forward, crossing into the following line.
func (b *buffer) next(p pos) (pos, bool) {
	if p.col+1 < len(b.lines[p.line]) {
		return pos{p.line, p.col + 1}, true
	}
	if p.line+1 < len(b.lines) {
		return pos{p.line + 1, 0}, true
	}
	return p, false
}

func (b *buffer) prev(p pos) (pos, bool) {
	if p.col > 0 {
		return pos{p.line, p.col - 1}, true
	}
	if p.line > 0 {
		return pos{p.line - 1, b.lastCol(p.line - 1)}, true
	}
	return p, false
}

func (b *buffer) blank(l int) bool {
	for _, c := range b.lines[l] {
		if strings.TrimSpace(c.text) != "" {
			return false
		}
	}
	return true
}

// text returns the selected characters between two inclusive positions.
func (b *buffer) text(from, to pos, linewise bool) string {
	if to.before(from) {
		from, to = to, from
	}

	var out strings.Builder
	for l := from.line; l <= to.line; l++ {
		cells := b.lines[l]
		start, end := 0, len(cells)
		if !linewise && l == from.line {
			start = min(from.col, end)
		}
		if !linewise && l == to.line {
			end = min(to.col+1, end)
		}

		for _, c := range cells[start:end] {
			out.WriteString(c.text)
		}
		if l < to.line || linewise {
			out.WriteByte('\n')
		}
	}
	return out.String()
}

// matches returns the cell indices where query starts in a line. A query
// without upper case letters matches case-insensitively.
func (b *buffer) matches(l int, query string) []int {
	fold := strings.ToLower(query) == query

	var (
		plain   strings.Builder
		offsets []int
	)
	for _, c := range b.lines[l] {
		offsets = append(offsets, plain.Len())
		if fold {
			plain.WriteString(strings.ToLower(c.text))
		} else {
			plain.WriteString(c.text)
		}
	}

	var found []int
	text := plain.String()
	for from := 0; from < len(text); {
		index := strings.Index(text[from:], query)
		if index < 0 {
			break
		}
		at := from + index
		found = append(found, sort.SearchInts(offsets, at+1)-1)
		from = at + 1
	}
	return found
}

type class int

const (
	classSpace class = iota
	classPunct
	classWord
	classEmpty
)

// class groups cells the way Vim splits words; with big set, WORDs.
func (b *buffer) class(p pos, big bool) class {
	cells := b.lines[p.line]
	if len(cells) == 0 {
		return classEmpty
	}

	r := []rune(cells[p.col].text)[0]
	switch {
	case unicode.IsSpace(r):
		return classSpace
	case big || r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r):
		return classWord
	default:
		return classPunct
	}
}
