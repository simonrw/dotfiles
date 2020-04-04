#!/bin/sh

set -e

killall -v polybar >/dev/null 2>&1 || true

# Set on both screens
if type "xrandr"; then
    for m in $(xrandr --query | grep " connected" | cut -d" " -f 1); do
        MONITOR=$m polybar classic &
    done
else
    polybar classic &
fi
