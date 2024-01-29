#!/bin/bash

kscreen-doctor output.eDP.disable
sleep 1
kscreen-doctor output.eDP.enable

notify-send "Displays reset"
