# start X if on first terminal
alias startx='startx &> ~/.xlog'
# [[ -z $SSH_CLIENT && -z $DISPLAY && $XDG_VTNR -eq 1 ]] && echo "Check storage" && sleep 3 && exec startx
[[ $(tty) != "/dev/tty1"  && -z $TMUX ]] && tmux && exit 0

# vim mode
export KEYTIMEOUT=1
bindkey -v

# run less on null read cmd
# < file.txt
export READNULLCMD=less

#sshrc pathprefix
PATHPREFIX=$HOME
if [[ ! -z "$SSHHOME" ]]; then
    PATHPREFIX=$SSHHOME/.sshrc.d
fi

# Source zsh configs
for file in $PATHPREFIX/.zsh/**/*.zsh; do
  source $file
done

fortune | cowsay -f $(ls /usr/share/cows | shuf -n1)
if [[ $( date +%A ) != "Friday" ]]; then echo "Its not Friday :("; else echo "Yea Friday!"; fi

# base16 colors
#BASE16_SHELL="$HOME/.config/base16-shell/base16-bespin.dark.sh"
#[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL
