#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import division, print_function, absolute_import
import argparse
import logging
import tempfile
import fitsio
import subprocess as sp
from contextlib import contextmanager

'''
* If single image:
    - source detect and crossmatch externally
* If catalogue:
    - crossmatch externally
* If catalogue and external catalogue
    - crossmatch internally
'''

logging.basicConfig(
    level='INFO', format='%(asctime)s : %(message)s')
logger = logging.getLogger(__name__)


def is_image(filename):
    logger.info('is_image')
    with fitsio.FITS(filename) as infile:
        return not len(infile) > 1


def run(*cmd):
    str_cmd = list(map(str, cmd))
    logger.debug('Running command %s', ' '.join(str_cmd))
    sp.check_call(str_cmd)


@contextmanager
def source_detect(image_filename):
    logger.info('source_detect')

    with fitsio.FITS(image_filename) as infile:
        image_hdu = infile[0]
        dims = image_hdu.get_info()['dims']
        scaling_factor = dims[0] / 2048

    with tempfile.NamedTemporaryFile(suffix='.fits') as outfile:
        run('imcore', image_filename, 'noconf', outfile.name, 2 * scaling_factor, 5,
            '--noell', '--filtfwhm', 1.8 * scaling_factor)
        outfile.seek(0)

        yield outfile.name


def _stilts(task, command):
    cmd = ['stilts', task] + list(command)
    run(*cmd)


def stilts(task, **kwargs):
    output = kwargs.pop('output', None)
    remapping = {
        '_in': 'in',
    }
    ignore_keys = {'output'}
    items = [(remapping.get(row[0], row[0]).replace('_', '.'), str(row[1]))
             for row in kwargs.items()
             if row[0] not in ignore_keys]
    args = ['='.join(row) for row in items]
    if output is not None:
        if output.use_gui():
            args.append('omode=swing')
        else:
            args.append('out={}'.format(output.filename))

    _stilts(task, args)


@contextmanager
def external_crossmatch(catalogue_filename):
    logger.info('external_crossmatch')
    with tempfile.NamedTemporaryFile(suffix='.fits') as outfile:
        stilts('cdsskymatch',
               cdstable='II/246/out',
               find='best',
               _in='{}#apm-binarytable'.format(catalogue_filename),
               ra='radiansToDegrees(RA)',
               dec='radiansToDegrees(DEC)',
               radius=10.,
               out=outfile.name,
               )
        outfile.seek(0)
        yield outfile.name


def get_central_coordinates(filename):
    with fitsio.FITS(filename) as hdulist:
        hdu = hdulist['apm-binarytable']
        header = hdu.read_header()

    return (header['TCRVL3'], header['TCRVL6'])


class STILTSOutput(object):

    def __init__(self, typ):
        self.typ = typ

    def use_gui(self):
        return self.typ.lower() in {'swing', 'gui'}

    @property
    def filename(self):
        return self.typ

    def should_render(self):
        return self.typ is not None


def visualise_matchfile(matchfile, sources_file, histogram, doughnut):
    logger.info('visualise_matchfile')
    logger.debug('Matchfile: %s', matchfile)

    centre_ra, centre_dec = get_central_coordinates(sources_file)

    doughnut_output = STILTSOutput(doughnut)
    histogram_output = STILTSOutput(histogram)

    if doughnut_output.should_render():
        stilts('plot2sky',
               _in='{}#1'.format(matchfile),
               lon='radiansToDegrees(RA)',
               lat='radiansToDegrees(DEC)',
               layer='mark',
               size=5,
               shading='aux',
               aux='angDist',
               auxmap='viridis',
               auxmin=0.,
               auxmax=1.,
               auxlabel='Angular separation',
               output=doughnut_output,
               xpix=1024,
               ypix=1024,
               )

    if histogram_output.should_render():
        stilts('plot2plane',
               layer='mark',
               _in='{}#1'.format(matchfile),
               x='skyDistanceDegrees(radiansToDegrees(RA), radiansToDegrees(DEC), {ra2}, {dec2})'.format(
                   ra2=centre_ra, dec2=centre_dec),
               y='angDist',
               xlabel='R [deg]',
               ylabel='Angular separation ["]',
               grid='true',
               output=histogram_output,
               xpix=1024,
               ypix=1024,
               size=5,
               ymin=0,
               ymax=10,
               )


def main(args):
    if args.verbose:
        logger.setLevel('DEBUG')
    logger.debug(args)

    if args.output_doughnut is None and args.output_histogram is None:
        raise ValueError('--oh and/or --od must be supplied')

    if is_image(args.filename):
        with source_detect(args.filename) as sources_file:
            with external_crossmatch(sources_file) as matchfile:
                visualise_matchfile(matchfile, sources_file,
                                    histogram=args.output_histogram, doughnut=args.output_doughnut)
    else:
        with external_crossmatch(args.filename) as matchfile:
            visualise_matchfile(matchfile, args.filename,
                                histogram=args.output_histogram, doughnut=args.output_doughnut)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('filename')
    parser.add_argument('-D', '--output-doughnut', required=False)
    parser.add_argument('-H', '--output-histogram', required=False)
    parser.add_argument('-v', '--verbose', action='store_true')
    main(parser.parse_args())
