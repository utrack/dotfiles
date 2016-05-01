alias psa="ps aux"
alias psg="ps aux | grep "


alias svim='sudoedit'
alias pacman='sudo pacman'
alias _=sudo
alias pls=sudo
alias md='mkdir -p'
alias blaze='chmod 420'

alias -r gief='pacman -S'
alias -r giefup='pacman -Syu'

alias gits='git status'
alias gitc='git commit'
alias gsta='git stash'
alias gstd='git stash drop'
alias gstp='git stash pop'
alias gsts='git stash show --text'
alias gite='vim `git ls-files -m` -p'

alias vif='vim $(fzf)'
alias gvf='gvim $(fzf)'

alias e="emacsclient -t"      # emacs terminal
alias ec="emacsclient -c -n"  # gui emacsclient

alias -r rf="rm -rf"
alias -r srf="sudo rm -rf"

alias -g g="| grep"
alias -g l="| less"

alias fuck='sudo $(fc -ln -1)'
alias ']'='open'

alias ls='ls --color=auto'
alias ks='ls'
alias ll='ls -alF'
alias la='ls -A'
alias lla='ls -lA'
alias l='ls -CF'

alias vi='vim'

alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias gz='tar -zcvf'
alias ugz='tar zxvf'

alias gop='export GOPATH=`(pwd)`:`(pwd)`/vendor'

alias cal='cal -m'
alias cp='cp -vr'
