#!/usr/bin/env zsh

if ! [[ -v LANG ]]; then
    export LANG='en_US.UTF-8'
fi

export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'


export XDG_DATA_HOME=$HOME/.local/share:/var/lib/flatpak/exports/share
export XDG_CONFIG_HOME="$HOME/.config"

export GOPATH="$HOME/go"
# local binaries
#export PATH="/usr/local/bin:/usr/local/sbin:$PATH"
export PATH="$HOME/.bin:$GOPATH/bin:$PATH"
