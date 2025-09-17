#!/bin/sh

stow bin dconf doomemacs git gtk i3 kde nnn nushell picom psd psql starship tmux vim xcompose xresources zellij zsh
# nnn plugins
sh -c "$(curl -Ls https://raw.githubusercontent.com/jarun/nnn/master/plugins/getplugs)"
# zprezto (custom)
git submodule update --init --recursive
# make sure kde starts itself via the script (so i3 can start)
kwriteconfig5 --file startkderc --group General --key systemdBoot false

