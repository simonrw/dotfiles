---
allowed-tools: Bash(jj new:*), Bash(jj status:*), Bash(jj diff:*), Bash(jj current-bookmark:*), Bash(jj diff-base:*), Bash(jj log:*), Bash(jj commit:*), Bash(jj describe:*), Bash(jj show:*)
description: Create a commit with jj
---

## Context

- Current status: !`jj status`
- Current git diff: !`jj diff`
- Current branch: !`jj current-bookmark`
- Diff from main branch: !`jj diff-base`
- Recent commits: !`jj log`

## Task

Based on the above changes, create a single commit. With jj this is normally done by calling `jj describe` to update the message of the current commit, then `jj new` to create a new empty commit on top of the previous one.
