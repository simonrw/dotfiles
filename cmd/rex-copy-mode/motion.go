package main

// wordForward implements Vim's w and W: it stops at the next word start or empty line.
func (b *buffer) wordForward(p pos, big bool) pos {
	q := p
	if c := b.class(q, big); c == classWord || c == classPunct {
		for {
			n, ok := b.next(q)
			if !ok {
				return q
			}
			crossed := n.line != q.line
			q = n
			if crossed || b.class(q, big) != c {
				break
			}
		}
	} else {
		n, ok := b.next(q)
		if !ok {
			return q
		}
		q = n
	}

	for b.class(q, big) == classSpace {
		n, ok := b.next(q)
		if !ok {
			return q
		}
		q = n
	}
	return q
}

// wordEnd implements Vim's e and E: it moves to the last character of the next word.
func (b *buffer) wordEnd(p pos, big bool) pos {
	q, ok := b.next(p)
	if !ok {
		return p
	}
	for c := b.class(q, big); c == classSpace || c == classEmpty; c = b.class(q, big) {
		if q, ok = b.next(q); !ok {
			return q
		}
	}

	c := b.class(q, big)
	for {
		n, ok := b.next(q)
		if !ok || n.line != q.line || b.class(n, big) != c {
			return q
		}
		q = n
	}
}

// wordBackward implements Vim's b and B: it moves to the start of the previous word or empty line.
func (b *buffer) wordBackward(p pos, big bool) pos {
	q, ok := b.prev(p)
	if !ok {
		return p
	}
	for b.class(q, big) == classSpace {
		if q, ok = b.prev(q); !ok {
			return q
		}
	}

	c := b.class(q, big)
	if c == classEmpty {
		return q
	}
	for {
		n, ok := b.prev(q)
		if !ok || n.line != q.line || b.class(n, big) != c {
			return q
		}
		q = n
	}
}

// paragraphForward implements Vim's }: it moves to the next blank line after a paragraph.
func (b *buffer) paragraphForward(p pos) pos {
	l := p.line
	for l < len(b.lines) && b.blank(l) {
		l++
	}
	for l < len(b.lines) && !b.blank(l) {
		l++
	}

	if l == len(b.lines) {
		return b.end()
	}
	return pos{l, 0}
}

// paragraphBackward implements Vim's {: it moves to the previous blank line before a paragraph.
func (b *buffer) paragraphBackward(p pos) pos {
	l := p.line
	for l >= 0 && b.blank(l) {
		l--
	}
	for l >= 0 && !b.blank(l) {
		l--
	}
	return pos{max(l, 0), 0}
}

func (b *buffer) firstNonBlank(l int) pos {
	for col := range b.lines[l] {
		if b.class(pos{l, col}, false) != classSpace {
			return pos{l, col}
		}
	}
	return pos{l, b.lastCol(l)}
}

// search finds the next match after p, wrapping around the buffer.
func (b *buffer) search(p pos, query string, forward bool) (pos, bool) {
	if query == "" {
		return p, false
	}

	count := len(b.lines)
	for step := 0; step <= count; step++ {
		offset := step
		if !forward {
			offset = -step
		}
		l := ((p.line+offset)%count + count) % count

		found := b.matches(l, query)
		if !forward {
			for i, j := 0, len(found)-1; i < j; i, j = i+1, j-1 {
				found[i], found[j] = found[j], found[i]
			}
		}
		for _, col := range found {
			candidate := pos{l, col}
			switch {
			case step == 0 && forward && !p.before(candidate),
				step == 0 && !forward && !candidate.before(p),
				step == count && forward && p.before(candidate),
				step == count && !forward && candidate.before(p):
				continue
			}
			return candidate, true
		}
	}
	return p, false
}
