#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import division, print_function, absolute_import
import argparse
import logging
import pymysql

logging.basicConfig(
    level='DEBUG', format='%(levelname)7s %(message)s')
logger = logging.getLogger(__name__)


def main(args):
    logger.debug(args)
    with pymysql.connect(user=args.user, host=args.host, db='ngts_ops',
            cursorclass=pymysql.cursors.DictCursor) as cursor:
        nrows = cursor.execute('select * from raw_image_list where image_id = %s',
                (args.imageid, ))
        if nrows != 1:
            raise RuntimeError('Number of rows found: {nrows}, should be 1'.format(
                nrows=nrows))
        result = cursor.fetchone()

    for key in args.keys:
        print('{key:>20s} {value}'.format( key=key, value=result[key]))

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-H', '--host', required=False, default='ngtsdb')
    parser.add_argument('-u', '--user', required=False, default='sw')
    parser.add_argument('imageid')
    parser.add_argument('-k', '--keys', help='Keys to return', nargs='+', required=True)
    main(parser.parse_args())
