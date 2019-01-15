MOZ_USE_XINPUT2=1

if [[ -z "$DOTPROFILEENV" ]]; then
    GOPATH="$HOME/go"
    PATH="/usr/local/bin:/usr/local/sbin:$PATH"
    PATH="$HOME/.bin:$GOPATH/bin:$PATH"
    DOTPROFILEENV=1
fi
