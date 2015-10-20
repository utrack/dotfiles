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
