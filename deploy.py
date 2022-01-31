#!/usr/bin/env python3


import argparse
from contextlib import contextmanager
import logging
import os
from pathlib import Path
import platform
import shutil
import subprocess as sp
from typing import Optional


logging.basicConfig(
    level=logging.WARNING, format="[%(asctime)s] %(levelname)7s:%(message)s"
)
logger = logging.getLogger("deploy")


class Deployer(object):
    @classmethod
    def deploy(cls) -> None:
        parser = argparse.ArgumentParser()
        parser.add_argument("-n", "--dry-run", action="store_true", default=False)
        parser.add_argument(
            "-H",
            "--homebrew",
            action="store_true",
            default=False,
            help="Install homebrew bundles (if on macos)",
        )
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

        self = cls(args)
        self.run()

    def __init__(self, args) -> None:
        self.args = args

    @property
    def dry_run(self):
        return self.args.dry_run

    @property
    def force(self):
        return self.args.force

    @property
    def compile(self):
        return self.args.compile

    @property
    def root(self):
        return self.args.root

    @property
    def homebrew(self):
        return self.args.homebrew

    def run(self) -> None:
        self.deploy_standard_dirs()
        self.deploy_dotconfig_files()
        if self.macos():
            if not self.homebrew:
                logger.warning(
                    "not installing homebrew packages as `-H/--homebrew` not specified"
                )
            else:
                homebrew = Homebrew()
                homebrew.install()
                homebrew.install_packages()

        if self.compile:
            self.install_custom_binaries()
        else:
            logger.warning(
                "`-c/--compile` not supplied, not compiling packages under `external`"
            )

    def deploy_standard_dirs(self, install_path: Optional[Path] = None) -> None:
        dirnames = [
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
            "oj",
            "taskwarrior",
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

            self._force_symlink_to(src, dst)
            logger.debug("--> linking complete")

    def deploy_dotconfig_files(self):
        macos_specific_locations = {"hookman"}
        for subdir in [
            "alacritty",
            "bat",
            "bspwm",
            "direnv",
            "hookman",
            "karabiner",
            "kitty",
            "nvim",
            "picom",
            "polybar",
            "rofi",
            "starship",
            "sxhkd",
        ]:
            macos_specific = self.macos() and subdir in macos_specific_locations
            self.deploy_dotconfig_file(subdir, macos_specific)

    def deploy_dotconfig_file(self, subdir, macos_specific):
        if macos_specific:
            root = Path.home() / "Library" / "Application Support"
        else:
            root = self.root.resolve() / ".config"

        srcs = Path.cwd().joinpath(subdir).glob("*")
        for src in srcs:
            dest = root.joinpath(subdir, src.name)
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

            self._force_symlink_to(src, dest)
            logger.debug("--> linking complete")

    def install_rust_packages(self):
        if not self._binary_exists("cargo"):
            logger.warning(
                "cannot find rust installation, skipping installing rust binaries"
            )
            return

        subdirs = [
            "external/git-identity-manager",
            "external/mkflashdriverepo",
            "external/hookman",
            "external/listprojects",
            "external/pm",
        ]
        for subdir in subdirs:
            if not os.path.isdir(subdir):
                continue
            cmd = ["cargo", "install", "--path", subdir]
            sp.run(cmd)

    def install_custom_binaries(self):
        if not self._binary_exists("go"):
            logger.warning("cannot find go compiler, skipping installing git-bug")
            return

        # git-bug
        logger.debug("compiling and installing external/git-bug")
        cmd = ["make", "-C", str(Path.cwd() / "external" / "git-bug"), "install"]
        sp.run(cmd)

        # gomodinit
        logger.debug("compiling and installing external/gomodinit")
        with self._chdir("external/gomodinit"):
            cmd = ["go", "install"]
            sp.run(cmd)

        # install-default-precommit
        logger.debug("compiling and installing external/install-default-precommit")
        with self._chdir("external/install-default-precommit"):
            cmd = ["go", "install"]
            sp.run(cmd)

        logger.debug("compiling and installing external/listprojects")
        with self._chdir("external/listprojects"):
            cmd = ["go", "install"]
            sp.run(cmd)

        self.install_rust_packages()

    def _deploy_single_file(self, src, dest):
        dest.parent.mkdir(parents=True, exist_ok=True)

        logger.info("deploying %s -> %s", src, dest)

        if dest.exists() and not self.force:
            logger.debug("--> path exists, skipping")
            return

        self._force_symlink_to(src, dest)
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

    @contextmanager
    def _chdir(self, path):
        oldpath = os.getcwd()
        try:
            os.chdir(path)
            yield
        finally:
            os.chdir(oldpath)

    def macos(self):
        return platform.system() == "Darwin"

    @staticmethod
    def _force_symlink_to(src, dest):
        """
        Ensures that the symlink clobbers the destination.
        """
        try:
            dest.symlink_to(src)
        except FileExistsError:
            try:
                shutil.rmtree(dest)
            except (OSError, NotADirectoryError):
                os.remove(dest)
            dest.symlink_to(src)


class Homebrew(object):
    def install(self):
        # TODO install homebrew itself. Note: this requires user interaction to disable SIP etc.
        pass

    def install_packages(self):
        # Check that we have a Brewfile in the current directory
        if not (Path.cwd() / "Brewfile").is_file():
            raise RuntimeError("cannot find brewfile in current directory")

        # Run the command
        cmd = ["brew", "bundle"]
        logger.info("deploying homebrew packages")
        logger.debug("running command: %s", cmd)
        sp.run(cmd)


if __name__ == "__main__":
    Deployer.deploy()
