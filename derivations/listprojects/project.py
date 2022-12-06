#!/usr/bin/env python
import os
import subprocess as sp
from pathlib import Path
import logging
import argparse

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger("project")


class Tmux:
    def __init__(self, path: Path) -> None:
        self.path = path
        self.session_name = f"{path.parent.name}/{path.name}"

    def activate(self) -> None:
        if self.in_tmux_session():
            if self.session_exists():
                self.switch_session()
            else:
                self.create_session()
                self.switch_session()
        else:
            self.create_session()
            self.attach_to_session()

    @staticmethod
    def in_tmux_session() -> bool:
        return os.getenv("TMUX") is not None

    def session_exists(self) -> bool:
        sessions = sp.check_output(["tmux", "ls", "-F", "#S"]).decode("utf-8").split("\n")
        for session in sessions:
            session = session.strip()
            if not session:
                continue
            if session == self.session_name:
                return True

        return False

    def switch_session(self) -> None:
        cmd = ["tmux", "switch-client", "-t", self.session_name]
        os.execvp(cmd[0], cmd)

    def create_session(self) -> None:
        cmd = ["tmux", "new-session", "-c", str(self.path), "-s", self.session_name, "-d"]
        sp.check_call(cmd)

    def attach_to_session(self) -> None:
        cmd = ["tmux", "attach", "-t", self.session_name]
        os.execvp(cmd[0], cmd)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", action="count", default=0)
    args = parser.parse_args()

    if args.verbose == 1:
        logger.setLevel(logging.INFO)
    elif args.verbose > 1:
        logger.setLevel(logging.DEBUG)

    dirs = [Path.home() / "dev", Path.home() / "work"]

    # get the list of projects
    find_cmd = ["fd", "--no-ignore", "--hidden", "--type", "d", ".git$"] + [str(d) for d in dirs]
    sed_cmd = ["sed", "-E", r"s#\.git/?$##"]
    select_cmd = ["fzf-tmux"] if Tmux.in_tmux_session() else ["fzf"]

    logger.info(f"Running {find_cmd} | {sed_cmd} | {select_cmd}")

    logger.debug("spawning find process")
    find_process = sp.Popen(find_cmd, stdout=sp.PIPE)
    logger.debug("spawning sed process")
    sed_process = sp.Popen(sed_cmd, stdin=find_process.stdout, stdout=sp.PIPE)
    logger.debug("spawning select cmd")
    select_child = sp.run(select_cmd, stdin=sed_process.stdout, stdout=sp.PIPE)

    logger.debug("all processes spawned")

    find_process.wait()
    sed_process.wait()

    if select_child.returncode == 130:
        # ctrl_c
        raise SystemExit(0)
    elif select_child.returncode != 0:
        stderr = select_child.stderr.decode('utf-8')
        raise ValueError(f"exit code {select_child.returncode}: {stderr}")

    selected_project = select_child.stdout.decode("utf-8").strip()
    selected_path = Path(selected_project).resolve()

    Tmux(selected_path).activate()
