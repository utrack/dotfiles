#!/usr/bin/env zsh

# run less on null read cmd
# < file.txt
export READNULLCMD=less

alias ff="fuck"
alias psa="ps aux"
alias psg="ps aux | grep "

alias gits="git status --short"

alias svim='sudoedit'
alias pacman='sudo pacman'
alias _=sudo
alias pls=sudo
alias md='mkdir -p'
alias blaze='chmod 420'

alias -r gief='yay -S'
alias -r giefup='yay -Syu'

alias kc="kubectl"
alias kcc="kubectl config use-context"

alias e="emacsclient -t"      # emacs terminal
alias ec="emacsclient -c -n"  # gui emacsclient
alias ecf="emacsclient -c -n \`fzf\`"  # ec with fzf as file chooser

alias -r rf="rm -rf"
alias -r srf="sudo rm -rf"

alias -g g="| grep"
alias -g l="| less"
alias -g jj="| jq '.'"

alias ls='ls --color=auto'
alias ks='ls'
alias ll='ls -alF'
alias la='ls -A'
alias lla='ls -lA'
alias l='ls -CF'
alias lt='exa --long --tree'

alias vi='vim'

alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias gz='tar -zcvf'
alias ugz='tar zxvf'

alias cal='cal -m'
alias cp='cp -vr'

alias ping='prettyping --nolegend'

alias archive='tar "-Izstd -19 -T0" -cf'
