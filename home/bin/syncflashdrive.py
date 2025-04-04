#!/usr/bin/env python


import subprocess as sp
import os.path as path
import os
import shlex
from contextlib import contextmanager


PROJECTS = [
    '~/work/MTC/data_munging/work-repo',
    '~/work/MTC/encompass/databases',
]


@contextmanager
def cd(path):
    cwd = os.getcwd()
    try:
        os.chdir(path)
        yield
    finally:
        os.chdir(cwd)


def run(cmd):
    split_cmd = shlex.split(cmd)
    sp.check_call(split_cmd)


def sync_database():
    run('git fetch origin')
    ncommits = fetch_ncommits()

    if ncommits == 0:
        return

    run('git merge --ff-only origin/master')


def fetch_ncommits():
    out = int(sp.check_output(shlex.split(
        'git rev-list --count HEAD..origin/master'
    )).strip().decode())

    return out



for project in PROJECTS:
    realpath = path.realpath(path.expanduser(project))

    with cd(realpath):
        sync_database()
