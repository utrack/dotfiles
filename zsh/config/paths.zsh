
#GOPATH
export GOPATH=$HOME/go
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
[[ ! -z $GOPATH ]] && export PATH=$PATH:$HOME/.bin:$GOPATH/bin

# cabal
export PATH=$PATH:~/.cabal/bin