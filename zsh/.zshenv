export DOTSPATH=$HOME/.dotfiles
export ZDOTSROOT=$DOTSPATH/zsh-internal

export ZDOTDIR=$ZDOTSROOT/zdots

# Source zsh configs
for file in $ZDOTSROOT/env/*.zsh; do
  source $file
done
