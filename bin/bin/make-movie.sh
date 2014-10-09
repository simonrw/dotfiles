#!/usr/bin/env bash

set -e

cpu_count() {
    python -c 'import multiprocessing; print multiprocessing.cpu_count()'
}

main() {
    mencoder 'mf://*.png' -mf w=800:h=600:fps=15:type=png -ovc x264 -x264encopts crf=18:nofast_pskip:nodct_decimate:nocabac:global_header:threads=$(cpu_count) -of lavf -lavfopts format=mp4 -o output.mp4
}

main
