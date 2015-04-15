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

Data = namedtuple('Data', ['mjd', 'x', 'y', 'airmass'])


def extract(fname):
    if fname.endswith('bz2'):
        with bz2.BZ2File(fname) as uncompressed:
            header = fits.getheader(uncompressed)
    else:
        header = fits.getheader(fname)
    if (header['imgclass'].lower() == 'science' and
        header['imgtype'].lower() == 'image'):
        return Data(header['mjd'], header['ag_errx'], header['ag_erry'],
                    header['airmass'])


def get_meta(files):
    pool = mp.Pool()
    extracted = list(filter(None, pool.map(extract, files)))
    extracted.sort(key=lambda d: d.mjd)

    x, y, airmass = list(map(np.array, [[getattr(row, key) for row in extracted]
                                        for key in ['x', 'y', 'airmass']]))
    return x, y, airmass


def main(args):
    logger.debug('%d files', len(args.filename))
    x, y, airmass = get_meta(args.filename)

    frame = np.arange(x.size)
    fig, axes = plt.subplots(2, 1, sharex=True)
    axes[0].plot(frame, x, '.', label='X')
    axes[0].plot(frame, y, '.', label='Y')
    axes[0].legend(loc='best')
    axes[0].set_ylabel(r'Correction / "')

    axes[1].plot(frame, airmass, '.')
    axes[1].set_ylabel(r'Airmass')

    axes[-1].set_xlabel(r'Frame number')
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
