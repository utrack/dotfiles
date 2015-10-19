# tmux color fix
alias tmux="TERM=screen-256color-bce tmux"

# start X if on first terminal
alias startx='startx &> ~/.xlog'
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
[[ $(tty) != "/dev/tty1"  && -z $TMUX ]] && tmux

export DOTSPATH=$HOME/.dotfiles

# Source zsh configs
for file in ~/.zsh/config/**/*.zsh; do
  source $file
done

# base16 colors
BASE16_SHELL="$HOME/.config/base16-shell/base16-bespin.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# vim mode
export KEYTIMEOUT=1
bindkey -v

echo "Storage:"
df -h | grep "^/dev/"

if [ -x /usr/games/cowsay -a -x /usr/games/fortune ]; then
  fortune | cowsay
fi

if [[ $( date +%A ) != "Friday" ]]; then echo "Its not Friday :("; else echo "Yea Friday!"; fi

. /etc/profile.d/vte.sh

#fasd
eval "$(fasd --init auto)"
#fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# command not found
. /etc/profile.d/cnf.sh
eval $(dircolors ~/.dircolors)


preexec () { print -rn -- $terminfo[el]; }
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
