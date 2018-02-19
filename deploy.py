#!/usr/bin/env python


from __future__ import print_function
import os
import sys
import argparse
import glob
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger('deploy')

EXCLUDE_LIST = [
    '.git',
    'external',
    'osx',
    '.',
    '..',
    'individual_files',
    '.DS_Store',
    '.bundle',
    'git-remote-hg',
    'plists',
    'colours',
    'provisioning',
    'dircolors',
    'emacs',
    'testing',
    'kitty',
]


def main(args):
    if args.verbose:
        logger.setLevel(logging.DEBUG)

    directories = (
        path for path in glob.iglob('*')
        if os.path.isdir(path)
        and path not in EXCLUDE_LIST
    )

    for directory in directories:
        create_link(directory, args.prefix, args.force, args.dry_run)

    deploy_specifics()


def deploy_specifics():
    deploy_kitty()


def deploy_kitty():
    dest = os.path.expanduser(
            os.path.join(
                '~', 'Library', 'Preferences', 'kitty', 'kitty.conf'))

    if os.path.exists(dest):
        raise ValueError('Kitty config file exists')

    src = os.path.join(
            os.path.dirname(__file__),
            'kitty', 'kitty', 'kitty.conf')

    containing_dir = os.path.dirname(dest)
    if not os.path.isdir(containing_dir):
        os.makedirs(containing_dir)

    os.symlink(src, dest)





def create_link(directory, prefix, force, dry_run):
    linkable_items = glob.glob(
        os.path.join(directory, '*'))
    source_paths = [
        os.path.realpath(path)
        for path in linkable_items
    ]
    destination_paths = [
        os.path.expanduser(
            os.path.join(
                prefix,
                '.{}'.format(os.path.basename(path))
            )
        ) for path in linkable_items]

    assert len(source_paths) == len(destination_paths)

    for src, dest in zip(source_paths, destination_paths):
        if os.path.exists(dest) and not force:
            logger.info('Path %s exists, and force not given; skipping',
                        dest)
            continue

        if dry_run:
            logger.debug('Would link %s => %s (dry-run)',
                         src, dest)
        else:
            logger.debug('Linking %s => %s', src, dest)
            os.symlink(src, dest)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--prefix', required=False, default='~')
    parser.add_argument('-f', '--force', required=False, action='store_true',
                        default=False)
    parser.add_argument('-v', '--verbose', required=False, action='store_true',
                        default=False)
    parser.add_argument('-n', '--dry-run', required=False, action='store_true',
                        default=False)
    main(parser.parse_args())
