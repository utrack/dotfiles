#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi


#lxsession &

# fonts (for you and bar)
xset +fp ~/.fonts
xset +fp /usr/share/fonts/local
xset +fp /home/u/.fonts
xset +fp /usr/share/fonts/TTF
xset +fp /usr/share/fonts/OTF
xset fp rehash 
#xcalib /home/sunn/x230.icm &
xsetroot -cursor_name left_ptr &

#xbindkeys &
# russian keymap
#setxkbmap -layout "us,ru" -option "grp:alt_shift_toggle,grp_led:scroll" &
setxkbmap -model pc105 -layout us,ru -option "grp:alt_shift_toggle,grp_led:scroll,compose:ralt,caps:escape"
#pulseaudio
start-pulseaudio-x11

#set xresource settings
xrdb $HOME/.Xresources &

#fix display thing
#$HOME/.bin/echoDisplay.sh &

#autorun
guake &
clipit &
hsetroot -fill ~/.wallpapers/Vi6Ml3t.jpg &

/usr/bin/xscreensaver -no-splash &

LD_PRELOAD=$HOME/.bin/skype-poll-fix.so skype &
psi-plus &

#kupfer &
#synapse &
clementine &

steam &

xflux -l 53 -g 45 &

eval $(gpg-agent --daemon) &

sxhkd &
#exec herbstluftwm
#exec subtle > ~/.subtle.log 2>1
exec bspwm

