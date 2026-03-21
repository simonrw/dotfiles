---
allowed-tools: Bash(git pr create:*), Bash(git diff:*), Bash(git log:*)
description: Create a LocalStack draft PR
---

# Context

LocalStack uses common patterns for its PRs. In particular labels. Also PRs should be always created as draft to prevent premature reviews.

# Task

Create a GitHub PR with the following command:

```
gh pr create --draft -l 'calver: patch' -l 'notes: skip' -l 'docs: skip'
```

and include the following information:

* title: summary of the change using service or context specific prefixes, like: cfn/, ci/ etc.
* body: fill the template from .github/PULL_REQUEST_TEMPLATE.md if it exists. Otherwise fill the following template:

```markdown
# Motivation

[Why the change is required]

# Changes

[What changed]

# Related

[Any related information like linear tickets]
```
