#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# mainly for Infinality fonts
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
  for f in /etc/X11/xinit/xinitrc.d/?* ; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# fonts (for you and bar)
xset +fp ~/.fonts
xset +fp /usr/share/fonts/local
xset +fp /home/u/.fonts
xset +fp /usr/share/fonts/TTF
xset +fp /usr/share/fonts/OTF
xset fp rehash &
xsetroot -cursor_name left_ptr &

xbindkeys &
# russian keymap,ralt as compose, caps = esc
#setxkbmap -layout "us,ru" -option "grp:alt_shift_toggle,grp_led:scroll" &
setxkbmap -model pc105 -layout us,ru -option "grp:alt_shift_toggle,grp_led:scroll,compose:ralt,ctrl:nocaps" &
xcape -e 'Control_L=Escape' &
#pulseaudio
start-pulseaudio-x11 &

#set xresource settings
xrdb -merge $HOME/.Xresources &

#autorun
urxvtd -q -f -o &
clipit &
xscreensaver -no-splash &

(( $+commands[skype] )) && skype &
(( $+commands[psi-plus] )) && psi-plus &

clementine &
dunst &

xflux -l 55 -g 37 &

eval $(gpg-agent --daemon) &


sxhkd &
. $HOME/.xinitrc.local &

emacs --daemon &

#exec herbstluftwm
#exec subtle > ~/.subtle.log 2>1
exec bspwm

