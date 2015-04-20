#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import division, print_function, absolute_import
import argparse
import logging
import pymysql

logging.basicConfig(level='INFO', format='%(levelname)7s %(message)s')
logger = logging.getLogger(__name__)


def main(args):
    if args.verbose:
        logger.setLevel('DEBUG')
    logger.debug(args)
    with pymysql.connect(user=args.user,
                         host=args.host,
                         db='ngts_ops',
                         cursorclass=pymysql.cursors.DictCursor) as cursor:
        logger.debug('Database connection open')
        nrows = cursor.execute(
            'select raw_image_list.*, autoguider_log.* from raw_image_list '
            'join autoguider_log on raw_image_list.image_id = autoguider_log.image_id '
            'where raw_image_list.image_id = %s', (args.imageid,))
        if nrows != 1:
            raise RuntimeError(
                'Number of rows found: {nrows}, should be 1'.format(nrows=nrows))
        result = cursor.fetchone()

    if args.keys is None:
        print('Available keys: {keys}'.format(keys='\n'.join(result.keys())))
    else:
        for key in args.keys:
            print('{key:} {value}'.format(key=key, value=result[key]))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-H', '--host', required=False, default='ngtsdb')
    parser.add_argument('-u', '--user', required=False, default='sw')
    parser.add_argument('imageid')
    parser.add_argument('-k', '--keys', help='Keys to return', nargs='+')
    parser.add_argument('-v', '--verbose', action='store_true')
    main(parser.parse_args())
