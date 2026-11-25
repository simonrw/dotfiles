#!/usr/bin/env python3
"""
PreToolUse hook for Bash that blocks commands which break under zsh with GNU coreutils, or which edit files in a way that can fail silently, and replies with how to fix them.

- BSD `sed -i ''`: GNU sed treats `''` as the script and fails.
- `$var:x`: zsh applies history modifiers (`:l` lowercases, `:h` / `:t` strip path parts), so `git show $rev:localstack/foo` becomes `git show <rev>ocalstack/foo`.
- Inline Python that replaces text and writes the file back without checking the match: a stale `old` string leaves the file unchanged and the command still succeeds.
"""

import json
import re
import sys

BSD_SED_INPLACE = re.compile(r"""(\bsed\b[^|;&\n]*?\s-i)\s*(?:''|"")(?=\s)""")

# single-quoted segments are matched first and kept, since zsh does not expand inside them
ZSH_MODIFIER = re.compile(r"""('[^']*')|(?<!\\)\$([A-Za-z_][A-Za-z0-9_]*|[0-9])(?=:[aAcefFghlpPqQrsStuwWx&])""")

PYTHON = re.compile(r"\bpython3?\b")
SILENT_REPLACE = re.compile(r"\.replace\(|\bre\.sub\(")
FILE_WRITE = re.compile(r"\.write_text\(|\.write\(")
MATCH_CHECK = re.compile(r"\bassert\b|\.count\(|\braise\b|\bsys\.exit\(")


def fix_zsh_modifier(match: re.Match) -> str:
    if match.group(1) is not None:
        return match.group(1)

    return "${" + match.group(2) + "}"


def is_unchecked_python_edit(command: str) -> bool:
    """
    Reports whether the command runs Python that replaces text and writes a file without any check that the text matched.
    """
    edits = PYTHON.search(command) and SILENT_REPLACE.search(command) and FILE_WRITE.search(command)

    return bool(edits) and not MATCH_CHECK.search(command)


def main():
    payload = json.load(sys.stdin)
    command = payload.get("tool_input", {}).get("command", "")

    problems = []

    fixed = BSD_SED_INPLACE.sub(r"\1", command)
    if fixed != command:
        problems.append("`sed -i ''` is BSD syntax; this machine has GNU sed, use `sed -i`.")

    rebraced = ZSH_MODIFIER.sub(fix_zsh_modifier, fixed)
    if rebraced != fixed:
        problems.append("The shell is zsh: `$var:x` applies a history modifier (`:l` lowercases, `:h`/`:t` strip path parts). Use `${var}:x`.")

    if is_unchecked_python_edit(command):
        problems.append(
            "This Python replaces text and writes the file without checking the match, so a stale `old` string would leave the file unchanged and still succeed. "
            "Use the Edit tool for targeted edits, or assert each match before writing, e.g. `assert s.count(old) == 1, old`."
        )

    if not problems:
        return

    message = "\n".join(problems)
    if rebraced != command:
        message += "\nCorrected command:\n" + rebraced

    print(message, file=sys.stderr)
    sys.exit(2)


if __name__ == "__main__":
    main()
