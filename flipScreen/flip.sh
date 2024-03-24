#!/bin/bash

if [ "$1" = "left" ]; then
    xrandr -o left
fi

if [ "$1" = "right" ]; then
    xrandr -o right
fi

if [ "$1" = "normal" ]; then
    xrandr -o normal
fi

if [ "$1" = "inverted" ]; then
    xrandr -o inverted
fi
