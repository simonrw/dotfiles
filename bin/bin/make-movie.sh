#!/usr/bin/env bash

set -e

main() {
    mencoder 'mf://*.png' -mf w=800:h=600:fps=15:type=png -ovc x264 -x264encopts crf=18:nofast_pskip:nodct_decimate:nocabac:global_header:threads=4 -of lavf -lavfopts format=mp4 -o output.mp4
}

main
