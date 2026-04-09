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

import os, json, re, argparse, csv, sys
from collections import defaultdict

CLAUDE_DIR = os.getenv("CLAUDE_CONFIG_DIR") or os.path.expanduser("~/.claude")
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
    parser.add_argument(
        "--no-duckdb", action="store_true", help="Output CSV to stdout instead of launching DuckDB"
    )
    args = parser.parse_args()

    allow_list = load_allow_list()
    projects_dir = os.path.join(CLAUDE_DIR, "projects")

    print("Loading sessions...", file=sys.stderr, flush=True)
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
            path = path.replace(os.path.expanduser("~"), "~")
            key = (tool_name, path, "")
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

    if not args.no_duckdb:
        launch_duckdb(rows)
    else:
        writer = csv.writer(sys.stdout)
        writer.writerow(["tool", "binary", "subcommand", "count", "done_pct", "allowed", "suggested_rule"])
        for tool, binary, sub, attempts, pct, allowed, rule in rows:
            writer.writerow([tool, binary, sub, attempts, pct, "yes" if allowed else "", rule])


def launch_duckdb(rows):
    import shutil, subprocess, tempfile

    duckdb_bin = shutil.which("duckdb")
    if not duckdb_bin:
        print("Error: duckdb binary not found on PATH", file=sys.stderr)
        sys.exit(1)

    with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
        writer = csv.writer(f)
        writer.writerow(["tool", "binary", "subcommand", "count", "done_pct", "allowed", "suggested_rule"])
        for tool, binary, sub, count, pct, allowed, rule in rows:
            writer.writerow([tool, binary, sub, count, pct, "yes" if allowed else "", rule])
        csv_path = f.name

    try:
        init_sql = f"CREATE TABLE permissions AS SELECT * FROM read_csv_auto('{csv_path}');"
        subprocess.run([duckdb_bin, "-cmd", init_sql], check=True)
    finally:
        os.unlink(csv_path)


if __name__ == "__main__":
    main()
