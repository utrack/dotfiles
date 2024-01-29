#!/usr/bin/env bash

if [ `pidof picom` ]; then
    echo "picom is there"
else
    picom -bc
fi
exit
#PASSWD=$(kdialog --password "sudo password required: battery-ac")
#echo "performance" | sudo tee /sys/devices/system/cpu/cpu{0..$(($(nproc)-1))}/cpufreq/energy_performance_preference
#echo "balance_power" | sudo tee /sys/devices/system/cpu/cpu{0..$(($(nproc)-1))}/cpufreq/energy_performance_preference
