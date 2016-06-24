#!/bin/bash

if [[ $(xrandr -q | grep " connected" | wc -l) == *2* ]]; then
    xrandr --output DP1 --auto --output eDP1 --auto --below DP1
    bspc monitor eDP1 -d web term file dev mus
    bspc monitor DP1 -d im foo foo2 steam
else
    xrandr --output DP1 --off --output eDP1 --auto
    bspc monitor eDP1 -d web term file dev mus im foo foo2 steam
fi

notify-send "Displays reset"
