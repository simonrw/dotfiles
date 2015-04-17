#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import division, print_function, absolute_import
import argparse
import logging
from astropy.io import fits
import numpy as np
import matplotlib.pyplot as plt


logging.basicConfig(
    level='DEBUG', format='%(levelname)7s %(message)s')
logger = logging.getLogger(__name__)


def main(args):
    logger.debug(args)
    image = fits.getdata(args.filename)
    if image.shape == (2048, 2088):
        image = image[:, 20:-20]

    f = np.abs(np.fft.fft(image))
    profile = f.mean(axis=0)

    x = np.fft.fftfreq(2048)
    ind = x > 0
    fig, axes = plt.subplots(1, 2)

    med_image = np.median(image)
    axes[0].imshow(image, vmin=0.8 * med_image, vmax=1.2 * med_image)

    axes[1].loglog(x[ind], profile[ind])
    
    if args.output is not None:
        fig.savefig(args.output)
    else:
        plt.show()

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('filename')
    parser.add_argument('-o', '--output', required=False)
    main(parser.parse_args())
