export DOTSPATH=$HOME/.dotfiles
export ZDOTDIR=$HOME/.zsh/files

if [[ ! -z "$ZSHENVEXP" ]]; then
    echo "no double exp"
    return
fi
export ZSHENVEXP=1
echo "123"
