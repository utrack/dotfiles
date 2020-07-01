#!/usr/bin/env zsh
set -euo pipefail

[[ $(tty) != "/dev/tty1"  && ! -v TMUX ]] && tmux && exit 0

if ! [[ -v ZDOTSROOT ]]; then
  echo "ZDOTSROOT is unset! Check dotfiles/zsh/.zshenv"
  return
fi

#sshrc pathprefix
# TODO remember sshrc
#PATHPREFIX=$HOME
# if [[ ! -z "$SSHHOME" ]]; then
#   PATHPREFIX=$SSHHOME/.sshrc.d
# fi

# Source zsh configs
for file in "$ZDOTSROOT"/rc/**/*.zsh; do
  source $file
done

fortune -a | cowsay -f $(ls /usr/share/cows | shuf -n1)
if [[ $( date +%A ) != "Friday" ]]; then echo "Its not Friday :("; else echo "Yea Friday!"; fi
