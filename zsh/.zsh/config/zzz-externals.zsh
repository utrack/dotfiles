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
if [[ -f /usr/share/fzf/completion.zsh ]]; then
  source /usr/share/fzf/completion.zsh
  source /usr/share/fzf/key-bindings.zsh
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
# Arch
if [[ -s '/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh' ]]; then
  source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi
# Ubuntu
if [[ -s '/usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh' ]]; then
  source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

eval $(thefuck --alias)
eval "$(direnv hook zsh)"
