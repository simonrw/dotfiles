#!/usr/bin/env bash
# Claude Code status line command
# Requires: jq

IFS=$'\t' read -r model used_pct used_tokens total_tokens < <(jq -r '[.model.display_name // "unknown", (.context_window.used_percentage // ""), (.context_window.total_input_tokens // ""), (.context_window.context_window_size // "")] | @tsv')

if [ -z "$used_pct" ]; then
  return
else
  pct_int=${used_pct%.*}
  used_tokens_int=${used_tokens:-0}
  if [ "$pct_int" -ge 90 ]; then
    color="\e[31m"
  elif [ -n "$used_tokens" ] && [ "$used_tokens_int" -ge 120000 ]; then
    color="\e[33m"
  else
    # below thresholds: show without color
    color="\e[0m"
  fi

  if [ -n "$used_tokens" ] && [ -n "$total_tokens" ]; then
    used_k=$((used_tokens / 1000))
    total_k=$((total_tokens / 1000))
    context_str="${used_k}k/${total_k}k"
  else
    context_str=$(printf "%.1f%%" "$used_pct")
  fi

  printf "%s | %b" "$model" "${color}Context: ${context_str}\e[0m"
fi
