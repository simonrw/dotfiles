#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import division, print_function, absolute_import
import argparse
import logging
import subprocess as sp
import glob

logging.basicConfig(
    level='INFO', format='%(levelname)7s %(message)s')
logger = logging.getLogger(__name__)


def main(args):
    if args.verbose:
        logger.setLevel('DEBUG')
    logger.debug(args)

    files = glob.glob('{dir}/IMAGE*.fits*'.format(dir=args.dir))
    cmd = ['find-good-image.py', ]
    cmd.extend(files)
    if args.output:
        cmd.extend(['-o', args.output])
    logger.debug('CMD: %s', ' '.join(cmd))
    sp.check_call(cmd)


if __name__ == '__main__':
    description = '''Plot the autoguider deltas and airmass for a single action'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('dir')
    parser.add_argument('-o', '--output', required=False)
    parser.add_argument('-v', '--verbose', action='store_true')
    main(parser.parse_args())
