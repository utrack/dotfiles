# prompt
setopt prompt_subst
setopt extendedglob

eval "$(starship init zsh)"

function p_right {
  local rtside
  #[[ -v SSH_CLIENT ]] && [[ -n $SSH_CLIENT ]] && rtside+="%F{cyan}%n@%m%f |"
  echo "$rtside %F{green}$(date "+%F %H:%M:%S")%f"
}

RPROMPT='$(p_right)'
