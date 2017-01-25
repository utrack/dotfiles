if [[ -s '/etc/profile.d/cnf.sh' ]]; then
  . /etc/profile.d/vte.sh
fi

#fasd
if (( $+commands[fasd] )) ; then
  eval "$(fasd --init auto)"
else
  echo "fasd not found!"
fi

#fzf
if [[ -f ~/.fzf.zsh ]]; then
  source ~/.fzf.zsh
else
  echo "fzf not found!"
fi

# command not found
# Debian/Ubuntu
if [[ -s '/etc/zsh_command_not_found' ]]; then
  source '/etc/zsh_command_not_found'
fi
# Arch
if [[ -s '/etc/profile.d/cnf.sh' ]]; then
  . /etc/profile.d/cnf.sh
fi

if [[ -s '~/.dircolors' ]]; then
  eval $(dircolors ~/.dircolors)
fi

preexec () { print -rn -- $terminfo[el]; }
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

eval $(thefuck --alias)
