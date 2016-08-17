#! /bin/sh

# launch or kill trayer
#. $HOME/.bin/panel/panel_settings
BARHEIGHT=16
CMD="trayer --align right --widthtype request --expand true --edge top --height ${BARHEIGHT} --alpha 0 --tint 0xeeeeee --transparent true --margin 250"

if [ $(pidof trayer) ] ; then
  killall trayer -u $USER;
else
  $CMD &
fi

