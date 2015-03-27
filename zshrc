# start X if on first terminal
[[ $(tty) = "/dev/tty1" ]] && exec startx

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# Golang paths
source $HOME/.dotfiles/godirsrc
[[ ! -z $GOPATH ]] && export PATH=$PATH:$HOME/.bin:$GOPATH/bin


# Compilation flags
export ARCHFLAGS="-arch x86_64"

export XDG_CONFIG_HOME="$HOME/.config"

if [ "$TERM" = "linux" ]; then
    echo -en "\e]P0222222" #black
    echo -en "\e]P8222222" #darkgrey
    echo -en "\e]P1803232" #darkred
    echo -en "\e]P9982b2b" #red
    echo -en "\e]P25b762f" #darkgreen
    echo -en "\e]PA89b83f" #green
    echo -en "\e]P3aa9943" #brown
    echo -en "\e]PBefef60" #yellow
    echo -en "\e]P4324c80" #darkblue
    echo -en "\e]PC2b4f98" #blue
    echo -en "\e]P5706c9a" #darkmagenta
    echo -en "\e]PD826ab1" #magenta
    echo -en "\e]P692b19e" #darkcyan
    echo -en "\e]PEa1cdcd" #cyan
    echo -en "\e]P7ffffff" #lightgrey
    echo -en "\e]PFdedede" #white
    clear #for background artifacting
fi

echo "Storage:"
df -h | grep "^/dev/"

if [ -x /usr/games/cowsay -a -x /usr/games/fortune ]; then
    fortune | cowsay
fi

if [[ $( date +%A ) != "Friday" ]]; then echo "Its not Friday :("; else echo "Yea Friday!"; fi


function lt() { ls -ltrsa "$@" | tail; }
function psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto; }
function fname() { find . -iname "*$@*"; }
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
eval $(dircolors ~/.dircolors)

#fasd
eval "$(fasd --init auto)"
