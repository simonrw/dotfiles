#!/usr/bin/env bash
# Claude Code status line command
# Requires: jq

IFS=$'\t' read -r model used_pct < <(jq -r '[.model.display_name // "unknown", (.context_window.used_percentage // "")] | @tsv')

if [ -z "$used_pct" ]; then
  return
else
  pct_int=${used_pct%.*}
  if [ "$pct_int" -ge 70 ]; then
    color="\e[31m"
  elif [ "$pct_int" -ge 60 ]; then
    color="\e[33m"
  else
    # I don't care about context unless it's above 60%
    return
  fi
  printf "%s | %b" "$model" "${color}Context: $(printf "%.1f%%" "$used_pct")\e[0m"
fi
