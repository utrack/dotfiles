#!/usr/bin/env zsh
set -euo pipefail

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
