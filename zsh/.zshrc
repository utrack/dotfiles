
# start X if on first terminal
alias startx='startx &> ~/.xlog'
[[ -z $SSH_CLIENT && -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
[[ $(tty) != "/dev/tty1"  && -z $TMUX ]] && tmux

export DOTSPATH=$HOME/.dotfiles
export ZDOTDIR=$HOME/.zsh/files
fpath=($ZDOTDIR $fpath)

# vim mode
export KEYTIMEOUT=1
bindkey -v

# run less on null read cmd
# < file.txt
export READNULLCMD=less

# Source zsh configs
for file in ~/.zsh/config/**/*.zsh; do
  source $file
done

# base16 colors
BASE16_SHELL="$HOME/.config/base16-shell/base16-bespin.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL


echo "Storage:"
df -h | grep "^/dev/"

if [ -x /usr/games/cowsay -a -x /usr/games/fortune ]; then
  fortune | cowsay
fi

if [[ $( date +%A ) != "Friday" ]]; then echo "Its not Friday :("; else echo "Yea Friday!"; fi
