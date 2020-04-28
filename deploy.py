#!/usr/bin/env python3


import argparse
from pathlib import Path
import platform
import logging
from typing import Optional
from contextlib import contextmanager
import subprocess as sp
import os


logging.basicConfig(
    level=logging.WARNING, format="[%(asctime)s] %(levelname)7s:%(message)s"
)
logger = logging.getLogger("deploy")


class Deployer(object):
    @classmethod
    def deploy(cls) -> None:
        parser = argparse.ArgumentParser()
        parser.add_argument("-n", "--dry-run", action="store_true", default=False)
        parser.add_argument("-f", "--force", action="store_true", default=False)
        parser.add_argument(
            "-c",
            "--compile",
            action="store_true",
            default=False,
            help="Compile packages under `external`",
        )
        parser.add_argument(
            "-r",
            "--root",
            required=False,
            default=Path.home(),
            type=Path,
            help="Install to a different root directory [default = home]",
        )
        parser.add_argument("-v", "--verbose", action="count")
        args = parser.parse_args()

        if args.verbose is not None:
            if args.verbose == 1:
                logger.setLevel(logging.INFO)
            elif args.verbose > 1:
                logger.setLevel(logging.DEBUG)

        logger.info("deploying dotfiles")
        logger.debug("debug logging enabled")

        self = cls(dry_run=args.dry_run, force=args.force, should_compile=args.compile, root=args.root)
        self.run()

    def __init__(self, dry_run: bool, force: bool, should_compile: bool, root: Path) -> None:
        self.dry_run = dry_run
        self.force = force
        self.compile = should_compile
        self.root = root

    def run(self) -> None:
        self.deploy_standard_dirs()
        self.deploy_dotconfig_files()
        if self.macos():
            self.deploy_kitty_config()
        if self.compile:
            self.install_rust_packages()
            self.install_custom_binaries()
        else:
            logger.warning(
                "`-c/--compile` not supplied, not compiling packages under `external`"
            )

    def deploy_standard_dirs(self, install_path: Optional[Path] = None) -> None:
        dirnames = [
            "sandbox",
            "vim",
            "zsh",
            "tmux",
            "bin",
            "conda",
            "emacs",
            "fish",
            "git",
            "hammerspoon",
            "i3",
            "ipython",
            "jupyter",
            "mutt",
            "pgcli",
            "postgresql",
            "xinitrc",
            "xresources",
            "xmodmap",
        ]
        for dirname in dirnames:
            src = Path.cwd().joinpath(dirname).resolve()
            self.deploy_standard_dir(src, install_path=install_path)

    def deploy_standard_dir(
        self, dirname: Path, install_path: Optional[Path] = None
    ) -> None:
        if install_path is None:
            install_path = self.root.resolve()

        logger.info("deploying %s", dirname)
        source = Path.cwd().resolve().joinpath(dirname)
        for src in source.glob("*"):
            dst = install_path.joinpath(f".{src.name}")
            logger.debug("- deploying %s to %s", src, dst)

            if dst.exists() and not self.force:
                logger.debug("--> path exists, skipping")
                continue

            dst.symlink_to(src)
            logger.debug("--> linking complete")

    def deploy_dotconfig_files(self):
        for subdir in [
            "direnv",
            "alacritty",
            "bspwm",
            "polybar",
            "sxhkd",
            "rofi",
            "picom",
            "nvim",
        ]:
            self.deploy_dotconfig_file(subdir)

    def deploy_dotconfig_file(self, subdir):
        srcs = Path.cwd().joinpath(subdir).glob("*")
        for src in srcs:
            dest = self.root.resolve().joinpath(".config", subdir, src.name)
            self._deploy_single_file(src, dest)

    def deploy_kitty_config(self):
        srcs = (Path.cwd() / "kitty" / "kitty").glob("*")
        dest_dir = self.root / "Library" / "Preferences" / "kitty"
        for src in srcs:
            logger.info("deploying %s", src)
            dest = dest_dir / src.name
            if dest.exists() and not self.force:
                logger.debug("--> path exists, skipping")
                continue

            dest.symlink_to(src)
            logger.debug("--> linking complete")

    def install_rust_packages(self):
        if not self._binary_exists("cargo"):
            logger.warning(
                "cannot find rust installation, skipping installing rust binaries"
            )
            return

        subdirs = ["external/git-identity-manager", "external/mkflashdriverepo"]
        for subdir in subdirs:
            cmd = ["cargo", "install", "--path", subdir]
            sp.run(cmd)

    def install_custom_binaries(self):
        # git-bug
        if not self._binary_exists("go"):
            logger.warning("cannot find go compiler, skipping installing git-bug")
            return

        cmd = ["make", "-C", str(Path.cwd() / "external" / "git-bug"), "install"]
        sp.run(cmd)

    def _deploy_single_file(self, src, dest):
        dest.parent.mkdir(parents=True, exist_ok=True)

        logger.info("deploying %s -> %s", src, dest)

        if dest.exists() and not self.force:
            logger.debug("--> path exists, skipping")
            return

        dest.symlink_to(src)
        logger.debug("--> linking complete")

    def _binary_exists(self, name):
        pathvar = os.environ["PATH"]
        for component in pathvar.split(":"):
            if not component:
                continue
            loc = Path(component)
            if not loc.is_dir():
                continue

            if loc / name:
                return True

        return False

    def macos(self):
        return platform.system() == "Darwin"


if __name__ == "__main__":
    Deployer.deploy()
