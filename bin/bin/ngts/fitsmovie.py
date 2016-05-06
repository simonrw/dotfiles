#!/usr/local/python/bin/python
# -*- coding: utf-8 -*-

from ds9 import *
import argparse

def main(args):
    files = args.file

    print('Loading files %s' % files)

    d = ds9()
    d.set('zscale')
    d.set('preserve scale')
    d.set('preserve pan')
    d.set('preserve zoom')

    for filename in files:
        d.set('fits {}'.format(filename))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('file', nargs='+')
    main(parser.parse_args())
