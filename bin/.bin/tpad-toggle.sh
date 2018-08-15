#!/bin/bash

declare -i ID
ID=`xinput list | sed -n 's/[^A-Za-z0-9]*\(touchpad\|glidepoint\|DLL07A0:01 044E:120B\)\s*id=\([0-9]\{1,2\}\).*$/\2/p'`
declare -i STATE
STATE=`xinput list-props $ID|grep 'Device Enabled'|awk '{print $4}'`
if [ $STATE -eq 1 ]
then
    xinput disable $ID
    # echo "Touchpad disabled."
    notify-send 'Touchpad' 'Disabled'
else
    xinput enable $ID
    # echo "Touchpad enabled."
    notify-send 'Touchpad' 'Enabled'
fi

