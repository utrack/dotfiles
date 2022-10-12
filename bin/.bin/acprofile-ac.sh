#!/usr/bin/env bash

if [ `pidof picom` ]; then
    echo "picom is there"
else
    picom -bc
fi
exit
