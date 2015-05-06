#!/usr/bin/env bash
set -e

sync() {
    fab update -H ngtshead.astro,astro,raspberrypi -P
}

sync
