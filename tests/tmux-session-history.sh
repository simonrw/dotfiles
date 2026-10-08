#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Stub external commands so no backend changes the active session.
cat > "$TMP/stub" <<'EOF'
#!/usr/bin/env bash
case "${0##*/}" in
  is-dark-theme) [[ "$TEST_THEME" == dark ]] ;;
  tmux) printf '123 oldest\n789 newest\n456 middle\n' ;;
  herdr)
    case "$*" in
      'status server --json') printf '{"session":null}\n' ;;
      'workspace list')
        printf '{"result":{"workspaces":[{"workspace_id":"w1","label":"oldest","focused":false},{"workspace_id":"w2","label":"newest","focused":true},{"workspace_id":"w3","label":"middle","focused":false}]}}\n'
        ;;
      'workspace focus w2') printf 'herdr\n' >> "$TEST_SELECTIONS" ;;
      *) exit 1 ;;
    esac
    ;;
  rex)
    case "$*" in
      'ls --json')
        printf '{"sessions":[{"session_id":"s2","label":"newest"},{"session_id":"s3","label":"middle"},{"session_id":"s1","label":"oldest"}]}\n'
        ;;
      'attach s2') printf 'rex\n' >> "$TEST_SELECTIONS" ;;
      *) exit 1 ;;
    esac
    ;;
  fzf)
    [[ "$1" == "--color=$TEST_THEME" ]] || exit 1
    [[ " $* " == *' --layout=default '* ]] || exit 1
    [[ " $* " == *' --no-sort '* ]] || exit 1
    printf '%s\n' "$TEST_THEME" >> "$TEST_LOG"
    cat > "$TEST_INPUT"
    head -n 1 "$TEST_INPUT"
    ;;
  tat) [[ "$*" == newest ]] && printf 'tmux\n' >> "$TEST_SELECTIONS" ;;
esac
EOF
chmod +x "$TMP/stub"
for command in is-dark-theme tmux herdr rex fzf tat; do
  ln -s stub "$TMP/$command"
done

unset REX_SESSION
export PATH="$TMP:$PATH" TEST_LOG="$TMP/themes" HERDR_ENV=1
export TEST_SELECTIONS="$TMP/selections" TEST_INPUT="$TMP/input"
export HERDR_PLUGIN_STATE_DIR="$TMP/state" HERDR_BIN_PATH="$TMP/herdr"
mkdir -p "$HERDR_PLUGIN_STATE_DIR"
printf '{"workspace":{"current":"w2","previous":"w3"},"tabs":{}}\n' > "$HERDR_PLUGIN_STATE_DIR/focus-history.json"
# Keep the inherited theme stale while switching the system theme between runs.
export FZF_DEFAULT_OPTS='--color dark --layout=reverse'
for SESSION_BACKEND in tmux herdr rex; do
  export SESSION_BACKEND
  for TEST_THEME in light dark light; do
    export TEST_THEME
    bash "$ROOT/.bin/tmux-session-history"
    if [[ "$SESSION_BACKEND" == tmux ]]; then
      printf 'newest\nmiddle\noldest\n' > "$TMP/expected-input"
    elif [[ "$SESSION_BACKEND" == herdr ]]; then
      printf 'newest\tw2\nmiddle\tw3\noldest\tw1\n' > "$TMP/expected-input"
    else
      printf 'newest\ts2\nmiddle\ts3\noldest\ts1\n' > "$TMP/expected-input"
    fi
    diff -u "$TMP/expected-input" "$TEST_INPUT"
  done
done
printf 'light\ndark\nlight\nlight\ndark\nlight\nlight\ndark\nlight\n' > "$TMP/expected"
diff -u "$TMP/expected" "$TEST_LOG"
printf 'tmux\ntmux\ntmux\nherdr\nherdr\nherdr\nrex\nrex\nrex\n' > "$TMP/expected-selections"
diff -u "$TMP/expected-selections" "$TEST_SELECTIONS"
printf 'Theme detection, backend ordering, and selection passed for all three backends.\n'
