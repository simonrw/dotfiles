#!/usr/bin/env python3
"""
PreToolUse hook for Bash that blocks commands which break under zsh with GNU coreutils, and replies with the corrected command.

- BSD `sed -i ''`: GNU sed treats `''` as the script and fails.
- `$var:x`: zsh applies history modifiers (`:l` lowercases, `:h` / `:t` strip path parts), so `git show $rev:localstack/foo` becomes `git show <rev>ocalstack/foo`.
"""

import json
import re
import sys

BSD_SED_INPLACE = re.compile(r"""(\bsed\b[^|;&\n]*?\s-i)\s*(?:''|"")(?=\s)""")

# single-quoted segments are matched first and kept, since zsh does not expand inside them
ZSH_MODIFIER = re.compile(r"""('[^']*')|(?<!\\)\$([A-Za-z_][A-Za-z0-9_]*|[0-9])(?=:[aAcefFghlpPqQrsStuwWx&])""")


def fix_zsh_modifier(match: re.Match) -> str:
    if match.group(1) is not None:
        return match.group(1)

    return "${" + match.group(2) + "}"


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

    if not problems:
        return

    message = "\n".join(problems) + "\nCorrected command:\n" + rebraced
    print(message, file=sys.stderr)
    sys.exit(2)


if __name__ == "__main__":
    main()
