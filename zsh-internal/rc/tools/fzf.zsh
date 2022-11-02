#!/usr/bin/env zsh

#fzf
if [[ -f /usr/share/fzf/completion.zsh ]]; then
  source /usr/share/fzf/completion.zsh
  source /usr/share/fzf/key-bindings.zsh
else
  echo "fzf not found!"
fi

