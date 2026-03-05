#!/usr/bin/env python3
"""
Analyze Claude Code session logs to surface candidate allow-list rules.

For each tool + command pattern, shows:
  - total attempts
  - completion rate (proxy for approval rate)
  - whether currently in allow list
  - suggested settings.json rule

Usage:
  python3 analyze_permissions.py [--project <name>] [--min-count <n>] [--tool <Tool>]
"""

import os, json, re, argparse
from collections import defaultdict

CLAUDE_DIR = os.path.expanduser("~/.claude")
SETTINGS_FILE = os.path.join(CLAUDE_DIR, "settings.json")


def load_allow_list():
    try:
        with open(SETTINGS_FILE) as f:
            s = json.load(f)
        return s.get("permissions", {}).get("allow", [])
    except Exception:
        return []


def normalize_command(cmd):
    """Extract (binary, subcommand) from a shell command string."""
    cmd = cmd.strip()
    if not cmd or cmd.startswith("#"):
        return ("(comment)", "")

    tokens = cmd.split()
    binary = tokens[0]

    if binary in ("sudo", "env", "time", "nice") and len(tokens) > 1:
        tokens = tokens[1:]
        binary = tokens[0]

    if "=" in binary:
        for t in tokens:
            if "=" not in t:
                binary = t
                tokens = tokens[tokens.index(t) :]
                break
        else:
            return ("(assignment)", "")

    sub = ""
    if len(tokens) > 1:
        candidate = tokens[1]
        if not candidate.startswith("/") and not candidate.startswith("~"):
            sub = candidate

    return (binary, sub)


def is_allowed(tool, binary, sub, allow_list):
    """Check if a command would be auto-allowed by the current allow list."""
    for rule in allow_list:
        m = re.match(r"^([\w_]+)\((.+)\)$", rule)
        if not m:
            if rule.endswith("*"):
                if tool.startswith(rule[:-1]):
                    return True
            elif rule == tool:
                return True
            continue
        rule_tool, pattern = m.group(1), m.group(2)
        if rule_tool != tool:
            continue
        if tool != "Bash":
            return True
        regex = re.escape(pattern).replace(r"\*", ".*")
        cmd_str = f"{binary} {sub}".strip() if sub else binary
        if re.match(regex, cmd_str):
            return True
    return False


def load_sessions(projects_dir, project_filter=None):
    tool_uses = {}
    tool_results = {}

    for proj in os.listdir(projects_dir):
        if project_filter and project_filter.lower() not in proj.lower():
            continue
        proj_path = os.path.join(projects_dir, proj)
        if not os.path.isdir(proj_path):
            continue
        for f in os.listdir(proj_path):
            if not f.endswith(".jsonl"):
                continue
            try:
                with open(os.path.join(proj_path, f)) as fh:
                    for line in fh:
                        try:
                            d = json.loads(line)
                            msg = d.get("message", {})
                            if not isinstance(msg, dict):
                                continue
                            role = msg.get("role")
                            content = msg.get("content", [])
                            if not isinstance(content, list):
                                continue
                            for item in content:
                                if not isinstance(item, dict):
                                    continue
                                if (
                                    role == "assistant"
                                    and item.get("type") == "tool_use"
                                ):
                                    uid = item.get("id")
                                    name = item.get("name", "")
                                    inp = item.get("input", {})
                                    if uid:
                                        tool_uses[uid] = (name, inp)
                                elif (
                                    role == "user" and item.get("type") == "tool_result"
                                ):
                                    uid = item.get("tool_use_id")
                                    is_error = item.get("is_error", False)
                                    if uid:
                                        tool_results[uid] = is_error
                        except Exception:
                            pass
            except Exception:
                pass

    return tool_uses, tool_results


def main():
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "--project", help="Filter to sessions matching this project name substring"
    )
    parser.add_argument(
        "--min-count", type=int, default=3, help="Minimum attempts to show (default: 3)"
    )
    parser.add_argument(
        "--tool", help="Filter to a specific tool (e.g. Bash, Edit, Read)"
    )
    parser.add_argument(
        "--show-allowed", action="store_true", help="Also show already-allowed patterns"
    )
    args = parser.parse_args()

    allow_list = load_allow_list()
    projects_dir = os.path.join(CLAUDE_DIR, "projects")

    print("Loading sessions...", flush=True)
    tool_uses, tool_results = load_sessions(projects_dir, args.project)

    stats = defaultdict(
        lambda: {"attempts": 0, "completed": 0, "errors": 0, "no_result": 0}
    )

    for uid, (tool_name, inp) in tool_uses.items():
        if args.tool and tool_name != args.tool:
            continue

        if tool_name == "Bash":
            cmd = inp.get("command", "")
            binary, sub = normalize_command(cmd)
            key = (tool_name, binary, sub)
        elif tool_name in ("Edit", "Write", "Read", "Glob", "Grep"):
            path = inp.get("file_path", inp.get("pattern", inp.get("path", "")))
            parts = path.replace(os.path.expanduser("~"), "~").split("/")
            path_key = "/".join(parts[:4]) + "/..." if len(parts) > 4 else path
            key = (tool_name, path_key, "")
        else:
            key = (tool_name, "", "")

        stats[key]["attempts"] += 1

        if uid in tool_results:
            if tool_results[uid]:
                stats[key]["errors"] += 1
            else:
                stats[key]["completed"] += 1
        else:
            stats[key]["no_result"] += 1

    rows = []
    for (tool, binary, sub), s in stats.items():
        attempts = s["attempts"]
        if attempts < args.min_count:
            continue
        completed = s["completed"]
        completion_pct = round(100 * completed / attempts) if attempts else 0
        allowed = is_allowed(tool, binary, sub, allow_list)
        if allowed and not args.show_allowed:
            continue

        if tool == "Bash" and binary not in ("(comment)", "(assignment)", ""):
            rule = f"Bash({binary} {sub}*)" if sub else f"Bash({binary}*)"
        elif tool != "Bash":
            rule = f"{tool}(*)"
        else:
            rule = ""

        rows.append((tool, binary, sub, attempts, completion_pct, allowed, rule))

    rows.sort(key=lambda r: (r[5], -r[3]))

    col_w = [6, 12, 14, 8, 7, 9, 30]
    header = f"{'Tool':<{col_w[0]}}  {'Binary':<{col_w[1]}}  {'Subcommand':<{col_w[2]}} {'Count':>{col_w[3]}}  {'Done%':>{col_w[2]}}  {'Count':>{col_w[3]}}  {'Done%':{col_w[4]}}  {'Allowed':<{col_w[5]}}  {'Suggested rule'}"
    print()
    print(header)
    print("-" * len(header))

    current_tool = None
    for tool, binary, sub, attempts, pct, allowed, rule in rows:
        if tool != current_tool:
            if current_tool is not None:
                print()
            current_tool = tool
        allowed_str = "yes" if allowed else ""
        print(
            f"{tool:<{col_w[0]}}  {binary:<{col_w[1]}}  {sub:<{col_w[2]}} {attempts:>{col_w[3]}}  {pct:>{col_w[2]}}  {attempts:>{col_w[3]}}  {pct:6}%  {allowed_str:<{col_w[5]}}  {rule}"
        )

    print()
    print(f"Tool invocations indexed: {len(tool_uses)}")
    print(f"Current allow list: {len(allow_list)} rules")
    print(
        f"Rows shown (>={args.min_count} attempts, not yet allowed): {sum(1 for r in rows if not r[5])}"
    )


if __name__ == "__main__":
    main()
