export DOTSPATH=$HOME/.dotfiles
export ZDOTDIR=$HOME/.zsh/files

if [[ ! -z "$ZSHENVEXP" ]]; then
    echo "no double exp"
    return
fi
export ZSHENVEXP=1

export FPATH=$HOME/.zsh/auload:/usr/share/zsh/site-functions:$FPATH

if [[ -z "$LANG" ]]; then
    export LANG='en_US.UTF-8'
fi

#
# Defines environment variables.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Ensure that a non-login, non-interactive shell has a defined environment.
# if [[ "$SHLVL" -eq 1 && ! -o LOGIN && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
#     echo "tryna source zprofile"
#     source "${ZDOTDIR:-$HOME}/.zprofile"
# fi

export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'


export XDG_DATA_HOME=$HOME/.local/share:/var/lib/flatpak/exports/share

# local binaries
if [[ -z "$DOTPROFILEENV" ]]; then
    export GOPATH=$HOME/go
    export PATH=/usr/local/bin:/usr/local/sbin:$PATH
    export PATH=$HOME/.bin:$GOPATH/bin:$PATH
fi

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

if (( $#commands[(i)lesspipe(|.sh)] )); then
    export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

MOZ_USE_XINPUT2=1

if [[ -z "$DOTPROFILEENV" ]]; then
    GOPATH="$HOME/go"
    PATH="/usr/local/bin:/usr/local/sbin:$PATH"
    PATH="$HOME/.bin:$GOPATH/bin:$PATH"
    DOTPROFILEENV=1
fi
