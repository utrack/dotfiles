# tmux color fix
alias tmux="TERM=screen-256color-bce tmux"

# start X if on first terminal
[[ $(tty) = "/dev/tty1" ]] && exec startx

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# base16 colors
BASE16_SHELL="$HOME/.config/base16-shell/base16-bespin.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# Golang paths
source $HOME/.dotfiles/godirsrc
[[ ! -z $GOPATH ]] && export PATH=$PATH:$HOME/.bin:$GOPATH/bin

# vim mode
export KEYTIMEOUT=1
bindkey -v

# Compilation flags
export ARCHFLAGS="-arch x86_64"

export XDG_CONFIG_HOME="$HOME/.config"

export VISUAL=vim
export EDITOR="$VISUAL"

echo "Storage:"
df -h | grep "^/dev/"

if [ -x /usr/games/cowsay -a -x /usr/games/fortune ]; then
  fortune | cowsay
fi

if [[ $( date +%A ) != "Friday" ]]; then echo "Its not Friday :("; else echo "Yea Friday!"; fi


function lt() { ls -ltrsa "$@" | tail; }
function psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto; }
function search() { find . -iname "*$@*" | less; }
function genpasswd {
local l=12 # default password lenght
if [ "$#" != "0" -a "$1" -gt 0 ]
then
  l=$1
fi
tr -dc A-Za-z0-9_ < /dev/urandom | head -c ${l} | xargs
}

# Sudo alias 
alias svim='sudoedit'
alias pacman='sudo pacman'
alias _=sudo
alias md='mkdir -p'

alias gits='git status'
alias gitc='git commit'
alias gsta='git stash'
alias gstd='git stash drop'
alias gstp='git stash pop'
alias gsts='git stash show --text'

alias -r rf="rm -rf"
alias -r srf="sudo rm -rf"
alias -g g="| grep"
alias -g l="| less"
alias fuck='sudo $(fc -ln -1)'
alias ']'='open'
alias ll='ls -alF'
alias la='ls -A'
alias lla='ls -lA'
alias l='ls -CF'
alias vi='vim'

alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dawt.useSystemAAFontSettings=true' 
export JAVA_FONTS=/usr/share/fonts/TTF

# Do not write command to history if it starts with space
setopt HIST_IGNORE_SPACE
setopt HIST_NO_STORE

# Dot not save duplicate commands
setopt HIST_SAVE_NO_DUPS
HISTFILE=$HOME/.zhistory
HISTSIZE=2000
SAVEHIST=2000

# One history for all sessions
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# Save the time and how long a command ran
#setopt EXTENDED_HISTORY

# Continue command from history (Up/Down)
autoload -Uz up-line-or-beginning-search
autoload -Uz down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '\eOA' up-line-or-beginning-search
bindkey '\e[A' up-line-or-beginning-search
bindkey '\eOB' down-line-or-beginning-search
bindkey '\e[B' down-line-or-beginning-search

# Undo last text change (e.g. completion)
bindkey '^_' undo



. /etc/profile.d/vte.sh

#fasd
eval "$(fasd --init auto)"
#fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
source /usr/share/doc/pkgfile/command-not-found.zsh
eval $(dircolors ~/.dircolors)


# Dirty hack over zpreszto's Bart theme to show vi-mode
function zle-line-init zle-keymap-select {
    PS1_2="${${KEYMAP/vicmd/NOR}/(main|viins)/INS}"
    PS1="%158>..>%{%F{red}%}%m%b%f%k%9(v. . %{%F{blue}%}%(?.[.%20(?.[%U.%S[))%7v%(?.].%20(?.%u].]%s))%b%f%k )$PS1_2| %{%F{default}%}%8~%b%f%k%<<%8v%143(l. . %{%F{default}%}%D%b%f%k)%151(l.. %{%F{red}%}%@%b%f%k)%9(v.
%{%F{blue}%}%(?.[.%20(?.[%U.%S[))%7v%(?.].%20(?.%u].]%s))%b%f%k.)
%m%# "
    zle reset-prompt
}
preexec () { print -rn -- $terminfo[el]; }
