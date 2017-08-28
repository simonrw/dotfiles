#!/usr/bin/env python


from __future__ import print_function
import subprocess as sp
import sys
import os
from contextlib import contextmanager


@contextmanager
def chdir(path):
    old_path = os.getcwd()
    try:
        os.chdir(path)
        yield
    finally:
        os.chdir(old_path)

def main():
    tag =  'srwalker101/dotfilestesting'

    if len(sys.argv) == 1:
        print('Program usage: {} {{build,shell,rmi,validate}}'.format(
            sys.argv[0]), file=sys.stderr)
        sys.exit(1)

    for arg in sys.argv[1:]:
        if arg == 'build':
            with chdir('testing'):
                run('docker', 'build', '-t', tag, '.')
        elif arg == 'shell':
            run('docker', 'run', '-it', '--rm', '-v', '{}:/dotfiles'.format(
                os.getcwd()), tag, 'zsh')
        elif arg == 'validate':
            validate()
        elif arg == 'rmi':
            run('docker', 'rmi', tag)
        else:
            print('Unexpected command: {}'.format(arg), file=sys.stderr)


def validate():
    test_paths = [
        '~/.zsh',
        '~/.vim/vimrc',
        '~/.config/flake8',
    ]

    for path in test_paths:
        fullpath = os.path.expanduser(path)
        if not os.path.exists(fullpath):
            print('Cannot find path: %s' % fullpath, file=sys.stderr)
            sys.exit(1)

    print('OK')


def run(*args):
    sp.check_call(list(map(str, args)))


if __name__ == '__main__':
    main()
