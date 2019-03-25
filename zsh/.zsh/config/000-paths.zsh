export FPATH=$PATHPREFIX/.zsh/auload:/usr/share/zsh/site-functions:$FPATH

if [[ -z "$LANG" ]]; then
    export LANG='en_US.UTF-8'
fi

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

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

if (( $#commands[(i)lesspipe(|.sh)] )); then
    export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

# better touchscreen support for Firefox
export MOZ_USE_XINPUT2=1

if [[ -z "$DOTPROFILEENV" ]]; then
    export GOPATH="$HOME/go"
    # local binaries
    export PATH="/usr/local/bin:/usr/local/sbin:$PATH"
    export PATH="$HOME/.bin:$GOPATH/bin:$PATH"
    export DOTPROFILEENV=1
fi
