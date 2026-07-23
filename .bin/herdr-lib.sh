# Shared helpers for herdr-aware session scripts. Source this file:
#   . "$(dirname "$0")/herdr-lib.sh"
# Requires: herdr, jq. Talks to the herdr server for the current pane's session
# (HERDR_SOCKET_PATH / HERDR_SESSION are inherited inside a herdr pane).

# Where `tl` (tmux-last) remembers the previously-focused workspace.
HERDR_LAST_WS_FILE="${HERDR_LAST_WS_FILE:-${XDG_CACHE_HOME:-$HOME/.cache}/herdr/last-workspace}"

# Print the active multiplexer for this shell: herdr | tmux | none
current_mux() {
  if [ -n "${HERDR_PANE_ID:-}" ]; then
    echo herdr
  elif [ -n "${TMUX:-}" ]; then
    echo tmux
  else
    echo none
  fi
}

# Emit workspaces as TSV rows: <workspace_id>\t<label>\t<focused>
# `herdr workspace list` returns {"result":{"workspaces":[...]}}; tolerate an
# unwrapped shape too via (.result // .).
herdr_workspaces() {
  herdr workspace list 2>/dev/null \
    | jq -r '(.result // .).workspaces[] | [.workspace_id, .label, (.focused | tostring)] | @tsv'
}

# Print the currently-focused workspace id (empty if none)
herdr_focused_ws() {
  herdr_workspaces | awk -F'\t' '$3 == "true" { print $1; exit }'
}

# Print a workspace id by exact label match (empty if none)
herdr_ws_by_label() {
  herdr_workspaces | awk -F'\t' -v want="$1" '$2 == want { print $1; exit }'
}

# Remember the currently-focused workspace so `tl` can jump back to it
herdr_record_last() {
  cur="$(herdr_focused_ws)"
  if [ -n "$cur" ]; then
    mkdir -p "$(dirname "$HERDR_LAST_WS_FILE")"
    printf '%s\n' "$cur" > "$HERDR_LAST_WS_FILE"
  fi
}

# Focus a workspace by id, recording the previous focus first (for `tl`)
herdr_focus() {
  herdr_record_last
  herdr workspace focus "$1"
}
