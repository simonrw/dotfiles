#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Stub external commands so neither backend changes the active session.
cat > "$TMP/stub" <<'EOF'
#!/usr/bin/env bash
case "${0##*/}" in
  is-dark-theme) [[ "$TEST_THEME" == dark ]] ;;
  tmux) printf '123 session\n' ;;
  herdr) [[ "$*" != 'workspace list' ]] || printf 'workspace\tid\n' ;;
  jq) cat ;;
  fzf)
    [[ "$1" == "--color=$TEST_THEME" ]] || exit 1
    printf '%s\n' "$TEST_THEME" >> "$TEST_LOG"
    cat
    ;;
  tat) : ;;
esac
EOF
chmod +x "$TMP/stub"
for command in is-dark-theme tmux herdr jq fzf tat; do
  ln -s stub "$TMP/$command"
done

export PATH="$TMP:$PATH" TEST_LOG="$TMP/themes" HERDR_ENV=1
# Keep the inherited theme stale while switching the system theme between runs.
export FZF_DEFAULT_OPTS='--color dark'
for SESSION_BACKEND in tmux herdr; do
  export SESSION_BACKEND
  for TEST_THEME in light dark light; do
    export TEST_THEME
    bash "$ROOT/.bin/tmux-session-history"
  done
done
printf 'light\ndark\nlight\nlight\ndark\nlight\n' > "$TMP/expected"
diff -u "$TMP/expected" "$TEST_LOG"
printf 'Theme detection passed for both backends.\n'
