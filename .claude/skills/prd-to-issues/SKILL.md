---
name: prd-to-issues
description: Break a PRD into independently-grabbable Linear sub-issues using tracer-bullet vertical slices. Use when user wants to convert a PRD to issues, create implementation tickets, or break down a PRD into work items.
---

# PRD to Issues

Break a PRD into independently-grabbable Linear sub-issues using vertical slices (tracer bullets).

## Required tools

- `mcp__linear-server__get_issue` — fetch the PRD issue
- `mcp__linear-server__save_issue` — create sub-issues (with `parentId`)
- `mcp__linear-server__list_teams` — discover the team to file against

## Process

### 1. Locate the PRD

Ask the user for the PRD's Linear issue identifier (e.g. TEAM-123).

If the PRD is not already in your context window, fetch it with `mcp__linear-server__get_issue`.

### 2. Explore the codebase (optional)

If you have not already explored the codebase, do so to understand the current state of the code.

### 3. Draft vertical slices

Break the PRD into **tracer bullet** issues. Each issue is a thin vertical slice that cuts through ALL integration layers end-to-end, NOT a horizontal slice of one layer.

Slices may be 'HITL' or 'AFK'. HITL slices require human interaction, such as an architectural decision or a design review. AFK slices can be implemented and merged without human interaction. Prefer AFK over HITL where possible.

<vertical-slice-rules>
- Each slice delivers a narrow but COMPLETE path through every layer (schema, API, UI, tests)
- A completed slice is demoable or verifiable on its own
- Prefer many thin slices over few thick ones
</vertical-slice-rules>

### 4. Quiz the user

Present the proposed breakdown as a numbered list. For each slice, show:

- **Title**: short descriptive name
- **Type**: HITL / AFK
- **Blocked by**: which other slices (if any) must complete first
- **User stories covered**: which user stories from the PRD this addresses

Ask the user:

- Does the granularity feel right? (too coarse / too fine)
- Are the dependency relationships correct?
- Should any slices be merged or split further?
- Are the correct slices marked as HITL and AFK?

Iterate until the user approves the breakdown.

### 5. Create the Linear sub-issues

For each approved slice, create a Linear issue using `mcp__linear-server__save_issue` with `parentId` set to the PRD issue identifier. This makes each slice a sub-issue of the PRD.

Create issues in dependency order (blockers first) so you can set `blockedBy` with real issue identifiers.

Use this issue description template:

```
## What to build

A concise description of this vertical slice. Describe the end-to-end behavior, not layer-by-layer implementation. Reference specific sections of the parent PRD rather than duplicating content.

## Acceptance criteria

- [ ] Criterion 1
- [ ] Criterion 2
- [ ] Criterion 3

## User stories addressed

Reference by number from the parent PRD:

- User story 3
- User story 7
```

Set the `blockedBy` field to the identifiers of any blocking issues, or omit if there are no blockers.

Do NOT close or modify the parent PRD issue.
