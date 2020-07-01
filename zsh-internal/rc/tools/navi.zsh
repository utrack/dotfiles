#!/usr/bin/env zsh
set -euo pipefail

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

