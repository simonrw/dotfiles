#!/usr/bin/env python
# -*- coding: utf-8 -*-

'''
Extract a single lightcurve from a pipeline run, along with its metadata

Required information:
    * post-sysrem flux
    * pre-sysrem flux
    * flux errors
    * ccdx
    * ccdy
    * skybkg
    * centdx
    * centdy
    * imagelist
    * catalogue
    * flags
'''

from __future__ import absolute_import, division, print_function
import argparse
from collections import namedtuple
import fitsio
import pymysql
import os
import numpy as np
import subprocess as sp
import logging
import sys

logging.basicConfig(level=logging.INFO, stream=sys.stderr,
                    format='%(name)s [%(levelname)s] (%(asctime)s): %(msg)s')
logger = logging.getLogger(os.path.basename(sys.argv[0]))

ObjectDescriptionBase = namedtuple(
    'ObjectDescriptionBase',
    'field obj_id tag sysrem_tag mapping'
)


class ObjectDescription(ObjectDescriptionBase):
    include_list = {
        'catalogue',
        'ccdx',
        'ccdy',
        'centdx',
        'centdy',
        'flags',
        'flux3',
        'flux3_err',
        'hjd',
        'imagelist',
        'skybkg',
        'sysrem_catalogue',
        'sysrem_flux3',
        'sysrem_imagelist',
    }

    def read_data(self):
        '''Reads the data and generates a new class with that info'''
        logger.info('Reading lightcurve data from disk')
        index = self.__compute_index()
        logger.debug('Object {} is at index {}'.format(self.obj_id, index))

        mapping = {}
        data_types = self.mapping.keys()
        for data_type in data_types:
            if data_type not in self.include_list:
                continue

            filename = self.mapping[data_type]

            mapping[data_type] = self.__extract_data(data_type, filename, index)
            logger.debug('Read {}'.format(data_type))

        return Object(self.field, self.obj_id, mapping)

    def __compute_index(self):
        catalogue_filename = self.mapping['catalogue']
        with fitsio.FITS(catalogue_filename) as infile:
            obj_id = [int(obj_id.decode()) for obj_id in
                      infile['catalogue']['obj_id'].read()]

        return obj_id.index(self.obj_id)

    @staticmethod
    def __extract_data(data_type, filename, index):
        if data_type.lower() in {'imagelist', 'sysrem_imagelist'}:
            return imagelist_from_file(filename)
        elif data_type.lower() in {'catalogue', 'sysrem_catalogue'}:
            return catalogue_from_file(filename, index)
        else:
            return array_from_file(filename, index)


class Object(object):

    def __init__(self, field, obj_id, mapping):
        self.field = field
        self.obj_id = obj_id
        self.mapping = mapping

    def save_as(self, filename):
        logger.info('Writing output to {}'.format(filename))
        data_types = sorted(self.mapping.keys())
        with fitsio.FITS(filename, 'rw', clobber=True) as outfile:
            for data_type in data_types:
                outfile.write(self.mapping[data_type], extname=data_type)


def fetch_file_locations(cursor, field, obj_id, tag, sysrem_tag):
    logger.debug('Querying the database for file locations')
    mapping = {}
    cursor.execute(
        '''select prod_id, directory, sub_type, filename
        from prod_dir
        join mergepipe_prod using (prod_id)
        join prod_cat using (prod_id)
        where field = %s
        and output_tag = %s
        ''', (field, tag)
    )
    rows = cursor.fetchall()
    if len(rows) == 0:
        raise RuntimeError('No entries found')

    for (prod_id, directory, sub_type, filename) in rows:
        mapping[sub_type.lower()] = os.path.join(directory, filename)

    logger.debug('Found pre-sysrem files under {}'.format(directory))

    cursor.execute(
        '''select sub_type, directory, filename
        from prod_dir
        join prod_cat using (prod_id)
        join sysrempipe_prod using (prod_id)
        where raw_prod_id = %s
        and output_tag = %s
        ''', (prod_id, sysrem_tag)
    )
    rows = cursor.fetchall()
    if len(rows) == 0:
        raise RuntimeError('No entries found')

    for sub_type, directory, filename in rows:
        path = os.path.join(directory, filename)
        mapping[sub_type.lower()] = path

    logger.debug('Found post-sysrem files under {}'.format(directory))

    return ObjectDescription(field, obj_id, tag, sysrem_tag, mapping)


def imagelist_from_file(filename):
    with fitsio.FITS(filename) as infile:
        raw = infile[1].read()

    keys = raw.dtype.names
    return {key.lower(): raw[key] for key in keys}


def catalogue_from_file(filename, index):
    with fitsio.FITS(filename) as infile:
        raw = infile[1].read()

    exclude_keys = {'OBJ_ID'}

    row = raw[index]
    keys = raw.dtype.names
    data = {key.lower(): np.array([row[key], ])
            for key in keys if key not in exclude_keys}
    return data


def array_from_file(filename, index):
    with fitsio.FITS(filename) as infile:
        return infile[0][index:index + 1, :].ravel()


def compress_file(filename):
    logger.info('Compressing file {filename} -> {filename}.bz2'.format(
        filename=filename)
    )
    command = ['bzip2', '--force', '--best', filename]
    sp.check_call(command)


def main(args):
    if args.verbose:
        logger.setLevel(logging.DEBUG)

    tag = args.tag
    if args.sysrem_tag:
        sysrem_tag = args.sysrem_tag
    else:
        sysrem_tag = tag

    with pymysql.connect(user='sw', host='ngtsdb', db='ngts_pipe') as cursor:
        file_mapping = fetch_file_locations(
            cursor=cursor,
            field=args.field,
            obj_id=args.obj_id,
            tag=tag,
            sysrem_tag=sysrem_tag
        )

    file_mapping.read_data().save_as(args.output)

    if not args.no_compress:
        compress_file(args.output)


if __name__ == '__main__':
    from textwrap import dedent
    description = 'Extract a single NGTS object'

    epilog = dedent('''
    
    Find a single NGTS object by field, object id and pipeline run tag and
    extract to a single file.  The file will have one HDU per data measurement.

    Post sysrem measurements are prefixed with 'sysrem_'.  

    Unless `--no-compress` is specified, the output file is compressed with
    bzip and the resulting output file will have .bz2 appended to the filename.

    ''')

    parser = argparse.ArgumentParser(description=description, epilog=epilog,
                                    formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('-f', '--field', required=True, help='NGTS field')
    parser.add_argument('-O', '--obj-id', required=True, type=int,
                        help='Integer object identifier in the pipeline run')
    parser.add_argument('-t', '--tag', required=True, help='Pre-sysrem analysis tag')
    parser.add_argument('-s', '--sysrem-tag', required=False,
                        help='Post-sysrem analysis tag (if different)')
    parser.add_argument('-o', '--output', required=True, help='Output filename')
    parser.add_argument('--no-compress', required=False,
                        action='store_true', default=False,
                        help='Do not compress file with bzip after creation')
    parser.add_argument('-v', '--verbose', action='store_true', default=False,
                       help='More verbose printing')
    main(parser.parse_args())
