#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import division, print_function, absolute_import
import argparse
import logging
from astropy.io import fits
import matplotlib.pyplot as plt
import numpy as np
from collections import namedtuple
import multiprocessing.dummy as mp
import bz2

plt.style.use('ggplot')

logging.basicConfig(level='DEBUG', format='%(levelname)7s %(message)s')
logger = logging.getLogger(__name__)

KEYS = ['mjd', 'x_delta', 'y_delta', 'x_corr', 'y_corr', 'x_error', 'y_error',
        'ag_apply', 'airmass', 'med_frame']


def extract(fname):
    if fname.endswith('bz2'):
        with bz2.BZ2File(fname) as uncompressed:
            with fits.open(uncompressed) as infile:
                header = infile[0].header
                med_image = np.median(infile[0].data)
    else:
        with fits.open(fname) as infile:
            header = infile[0].header
            med_image = np.median(infile[0].data)

    if (header['imgclass'].lower() == 'science' and
        header['imgtype'].lower() == 'image'):
        return {
            'mjd': header['mjd'],
            'x_delta': header['ag_deltx'],
            'y_delta': header['ag_delty'],
            'x_corr': header['ag_corrx'],
            'y_corr': header['ag_corry'],
            'x_error': header['ag_errx'],
            'y_error': header['ag_erry'],
            'ag_apply': bool(header['ag_apply']),
            'med_frame': med_image,
            'airmass': header['airmass'],
        }


def get_meta(files):
    pool = mp.Pool()
    extracted = list(filter(None, pool.map(extract, files)))
    extracted.sort(key=lambda d: d['mjd'])

    out = {key: np.asarray([row[key] for row in extracted]) for key in KEYS}
    return out


def main(args):
    logger.debug('%d files', len(args.filename))
    meta = get_meta(args.filename)

    mjd0 = int(meta['mjd'].min())
    mjd = meta['mjd'] - mjd0
    frame = np.arange(mjd.size)

    fig, axes = plt.subplots(5, 1, sharex=True)
    axes[0].plot(frame, meta['x_error'] + 1., '.', label=r'$X + 1$')
    axes[0].plot(frame, meta['y_error'] - 1., '.', label=r'$Y - 1$')
    axes[0].set_ylabel(r'Error / "')
    axes[0].set_ylim(-5, 5)

    axes[1].plot(frame, meta['x_corr'] + 1., '.', label=r'$X + 1$')
    axes[1].plot(frame, meta['y_corr'] - 1., '.', label=r'$Y - 1$')
    axes[1].set_ylabel(r'Correction / "')
    axes[1].set_ylim(-5, 5)

    axes[2].plot(frame, meta['x_delta'], '.', label='X')
    axes[2].plot(frame, meta['y_delta'], '.', label='Y')
    axes[2].set_ylabel(r'Delta / "')

    axes[-2].plot(frame, meta['med_frame'], '.')
    axes[-2].set_ylabel(r'Frame median')

    axes[-1].plot(frame, meta['airmass'], '.')
    axes[-1].set_ylabel(r'Airmass')

    for ax in axes[:-1]:
        ax.legend(loc='best')
        offset = 50
        ax.set_xlim(-offset, frame.max() + offset)

    axes[-1].set_xlabel('Frame')
    fig.tight_layout()

    if args.output is None:
        plt.show()
    else:
        fig.savefig(args.output, bbox_inches='tight')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('filename', nargs='+')
    parser.add_argument('-o', '--output', required=False, type=argparse.FileType('w'))
    main(parser.parse_args())
