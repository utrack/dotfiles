
#GOPATH
export GOPATH=$HOME/go

# local binaries
export PATH=$HOME/.bin:$PATH
[[ ! -z $GOPATH ]] && export PATH=$GOPATH/bin:$PATH

# cabal
export PATH=$PATH:$HOME/.cabal/bin