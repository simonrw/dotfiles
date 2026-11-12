package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
)

// row is the part of a logical line that fits on one screen row.
type row struct {
	line, start, end int
}

type model struct {
	buf  *buffer
	copy func(string) error

	width, height int
	rows          []row
	// lineRow holds the first row of each logical line.
	lineRow []int
	top     int

	cursor pos
	// want is the screen column vertical motions aim for, like Vim's curswant.
	want int

	anchor   *pos
	linewise bool

	count   int
	pending string

	searching     bool
	query         string
	searchForward bool
	lastQuery     string
	lastForward   bool

	message string
}

func newModel(buf *buffer, copy func(string) error) *model {
	last := len(buf.lines) - 1
	return &model{buf: buf, copy: copy, cursor: pos{last, 0}}
}

func (m *model) Init() tea.Cmd {
	return nil
}

func (m *model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		first := m.width == 0
		m.width, m.height = msg.Width, msg.Height
		m.layout()
		// Line the snapshot's last row up with the bottom, as the terminal showed it.
		if first {
			m.top = max(len(m.rows)-m.viewHeight(), 0)
		}
		m.scrollToCursor()

	case tea.KeyPressMsg:
		if m.searching {
			m.searchKey(msg)
			return m, nil
		}
		cmd := m.key(msg.String())
		m.scrollToCursor()
		return m, cmd
	}
	return m, nil
}

func (m *model) key(k string) tea.Cmd {
	m.message = ""
	count := m.count
	m.count = 0
	n := max(count, 1)

	if m.pending == "g" {
		m.pending = ""
		if k == "g" {
			m.goToLine(max(count, 1) - 1)
		}
		return nil
	}

	if digit, err := strconv.Atoi(k); err == nil && len(k) == 1 && (digit > 0 || count > 0) {
		m.count = min(count*10+digit, 1_000_000)
		return nil
	}

	switch k {
	case "h", "left":
		m.setCursor(pos{m.cursor.line, max(m.cursor.col-n, 0)})
	case "l", "right":
		m.setCursor(pos{m.cursor.line, min(m.cursor.col+n, m.buf.lastCol(m.cursor.line))})
	case "j", "down":
		m.moveRows(n)
	case "k", "up":
		m.moveRows(-n)

	case "w", "W", "b", "B", "e", "E", "{", "}":
		big := strings.ToUpper(k) == k
		motion := map[string]func(pos) pos{
			"w": func(p pos) pos { return m.buf.wordForward(p, big) },
			"b": func(p pos) pos { return m.buf.wordBackward(p, big) },
			"e": func(p pos) pos { return m.buf.wordEnd(p, big) },
			"{": m.buf.paragraphBackward,
			"}": m.buf.paragraphForward,
		}[strings.ToLower(k)]
		p := m.cursor
		for range n {
			p = motion(p)
		}
		m.setCursor(p)

	case "0", "home":
		m.setCursor(pos{m.cursor.line, 0})
	case "^":
		m.setCursor(m.buf.firstNonBlank(m.cursor.line))
	case "$", "end":
		m.setCursor(pos{m.cursor.line, m.buf.lastCol(m.cursor.line)})
		m.want = math.MaxInt

	case "g":
		m.pending = "g"
		m.count = count
	case "G":
		if count == 0 {
			count = len(m.buf.lines)
		}
		m.goToLine(count - 1)

	case "H":
		m.setRow(m.top + n - 1)
	case "M":
		m.setRow(m.top + (min(len(m.rows)-m.top, m.viewHeight())-1)/2)
	case "L":
		m.setRow(m.top + m.viewHeight() - n)

	case "ctrl+d":
		m.scroll(max(m.viewHeight()/2, 1))
	case "ctrl+u":
		m.scroll(-max(m.viewHeight()/2, 1))
	case "ctrl+f", "pgdown":
		m.scroll(m.viewHeight())
	case "ctrl+b", "pgup":
		m.scroll(-m.viewHeight())
	case "ctrl+e":
		m.top = min(m.top+n, max(len(m.rows)-1, 0))
		m.cursorIntoView()
	case "ctrl+y":
		m.top = max(m.top-n, 0)
		m.cursorIntoView()

	case "/", "?":
		m.searching = true
		m.query = ""
		m.searchForward = k == "/"
	case "n", "N":
		for range n {
			m.search(m.lastForward == (k == "n"))
		}

	case "v", "space":
		if m.anchor != nil && !m.linewise && k == "v" {
			m.anchor = nil
		} else if m.anchor == nil || k == "v" {
			m.startSelection(false)
		}
	case "V":
		if m.anchor != nil && m.linewise {
			m.anchor = nil
		} else {
			m.startSelection(true)
		}
	case "o":
		if m.anchor != nil {
			anchor := m.cursor
			m.setCursor(*m.anchor)
			m.anchor = &anchor
		}

	case "y", "enter":
		if m.anchor == nil {
			return nil
		}
		if err := m.copy(m.buf.text(*m.anchor, m.cursor, m.linewise)); err != nil {
			m.message = "Could not copy: " + err.Error()
			return nil
		}
		return tea.Quit

	case "esc":
		if m.anchor != nil {
			m.anchor = nil
			return nil
		}
		return tea.Quit
	case "q", "ctrl+c":
		return tea.Quit
	}
	return nil
}

func (m *model) searchKey(msg tea.KeyPressMsg) {
	switch msg.String() {
	case "enter":
		m.searching = false
		if m.query != "" {
			m.lastQuery = m.query
		}
		m.lastForward = m.searchForward
		m.search(m.lastForward)
		m.scrollToCursor()
	case "esc", "ctrl+c":
		m.searching = false
	case "backspace":
		if m.query == "" {
			m.searching = false
			return
		}
		runes := []rune(m.query)
		m.query = string(runes[:len(runes)-1])
	default:
		m.query += msg.Text
	}
}

func (m *model) search(forward bool) {
	p, ok := m.buf.search(m.cursor, m.lastQuery, forward)
	if !ok {
		m.message = "Pattern not found: " + m.lastQuery
		return
	}
	m.setCursor(p)
}

func (m *model) startSelection(linewise bool) {
	if m.anchor == nil {
		anchor := m.cursor
		m.anchor = &anchor
	}
	m.linewise = linewise
}

func (m *model) selected(p pos) bool {
	if m.anchor == nil {
		return false
	}

	from, to := *m.anchor, m.cursor
	if to.before(from) {
		from, to = to, from
	}
	if m.linewise {
		return from.line <= p.line && p.line <= to.line
	}
	return !p.before(from) && !to.before(p)
}

func (m *model) layout() {
	m.rows = m.rows[:0]
	m.lineRow = make([]int, len(m.buf.lines))
	for l, cells := range m.buf.lines {
		m.lineRow[l] = len(m.rows)
		start, width := 0, 0
		for i, c := range cells {
			if width+c.width > m.width && i > start {
				m.rows = append(m.rows, row{l, start, i})
				start, width = i, 0
			}
			width += c.width
		}
		m.rows = append(m.rows, row{l, start, len(cells)})
	}
}

func (m *model) viewHeight() int {
	return max(m.height-1, 1)
}

func (m *model) cursorRow() int {
	r := m.lineRow[m.cursor.line]
	for r+1 < len(m.rows) && m.rows[r+1].line == m.cursor.line && m.rows[r+1].start <= m.cursor.col {
		r++
	}
	return r
}

func (m *model) column(p pos, r int) int {
	width := 0
	for _, c := range m.buf.lines[p.line][m.rows[r].start:p.col] {
		width += c.width
	}
	return width
}

func (m *model) setCursor(p pos) {
	m.cursor = p
	if m.rows != nil {
		m.want = m.column(p, m.cursorRow())
	}
}

// setRow moves the cursor to a screen row, keeping the wanted column.
func (m *model) setRow(r int) {
	r = min(max(r, 0), len(m.rows)-1)
	target := m.rows[r]

	col, width := target.start, 0
	for i := target.start; i < target.end; i++ {
		if width > m.want {
			break
		}
		col = i
		width += m.buf.lines[target.line][i].width
	}
	m.cursor = pos{target.line, col}
}

func (m *model) moveRows(delta int) {
	m.setRow(m.cursorRow() + delta)
}

func (m *model) goToLine(l int) {
	m.setCursor(m.buf.firstNonBlank(min(max(l, 0), len(m.buf.lines)-1)))
}

func (m *model) scroll(delta int) {
	m.top = min(max(m.top+delta, 0), max(len(m.rows)-m.viewHeight(), 0))
	m.moveRows(delta)
}

func (m *model) scrollToCursor() {
	if m.rows == nil {
		return
	}
	r := m.cursorRow()
	m.top = min(m.top, r)
	m.top = max(m.top, r-m.viewHeight()+1)
}

func (m *model) cursorIntoView() {
	r := m.cursorRow()
	switch {
	case r < m.top:
		m.setRow(m.top)
	case r >= m.top+m.viewHeight():
		m.setRow(m.top + m.viewHeight() - 1)
	}
}

func (m *model) View() tea.View {
	if m.rows == nil {
		return tea.NewView("")
	}

	var out strings.Builder
	for i := range m.viewHeight() {
		if r := m.top + i; r < len(m.rows) {
			m.renderRow(&out, m.rows[r])
		}
		out.WriteString("\n")
	}
	status := m.status()
	out.WriteString(status)

	view := tea.NewView(out.String())
	view.AltScreen = true
	if m.searching {
		view.Cursor = tea.NewCursor(ansi.StringWidth(status), m.height-1)
	} else {
		r := m.cursorRow()
		view.Cursor = tea.NewCursor(m.column(m.cursor, r), r-m.top)
	}
	return view
}

// renderRow re-emits each cell with its captured style, reversed when selected.
func (m *model) renderRow(out *strings.Builder, r row) {
	cells := m.buf.lines[r.line]
	if r.start == r.end && m.selected(pos{r.line, 0}) {
		out.WriteString("\x1b[7m \x1b[0m")
		return
	}

	current := ""
	for col := r.start; col < r.end; col++ {
		c := cells[col]
		style := c.style
		if m.selected(pos{r.line, col}) {
			style += "\x1b[7m"
		}
		if style != current {
			out.WriteString("\x1b[0m" + style)
			current = style
		}
		out.WriteString(c.text)
	}
	out.WriteString("\x1b[0m")
}

var (
	modeStyle   = lipgloss.NewStyle().Bold(true)
	detailStyle = lipgloss.NewStyle().Faint(true)
	errorStyle  = lipgloss.NewStyle().Foreground(lipgloss.Red)
)

func (m *model) status() string {
	if m.searching {
		prompt := "?"
		if m.searchForward {
			prompt = "/"
		}
		return prompt + m.query
	}

	mode := "COPY"
	if m.anchor != nil {
		mode = "VISUAL"
		if m.linewise {
			mode = "V-LINE"
		}
	}
	left := modeStyle.Render(mode)
	if m.message != "" {
		left += " " + errorStyle.Render(m.message)
	}

	right := detailStyle.Render(fmt.Sprintf("%d/%d", m.cursor.line+1, len(m.buf.lines)))
	gap := m.width - ansi.StringWidth(left) - ansi.StringWidth(right)
	if gap < 1 {
		return ansi.Truncate(left, m.width, "")
	}
	return left + strings.Repeat(" ", gap) + right
}
