# Continue command from history (Up/Down)
autoload -Uz up-line-or-beginning-search
autoload -Uz down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '\eOA' up-line-or-beginning-search
bindkey '\e[A' up-line-or-beginning-search
bindkey '\eOB' down-line-or-beginning-search
bindkey '\e[B' down-line-or-beginning-search

# Undo last text change (e.g. completion)
bindkey '^_' undo
# Remove push-line,make ^Q work in vim
stty start '^-' stop '^-'
bindkey '^B' push-line
# reclaim ^s as well
stty stop undef
stty start undef
