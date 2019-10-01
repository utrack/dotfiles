# prompt
setopt prompt_subst
setopt extendedglob

# mode-aware arrow
function p_arrow {
  if [[ $KEYMAP = "vicmd" ]]; then
    echo "%F{magenta}»%f"
  else
    echo "%F{cyan}»%f"
  fi
}

# colored path
function p_colored_path {
  local slash="%F{cyan}/%f"
  echo "${${PWD/#$HOME/~}//\//$slash}"
}

# anchor, red if last command failed
function anchor {
  echo "%(?,%F{cyan},%F{red})λ"
}

# git info
function p_vcs {
  vcs_info
  echo $vcs_info_msg_0_
}

# environments:
#  - ssh
#  - virtualenv
export VIRTUAL_ENV_DISABLE_PROMPT=1
function p_envs {
  local envs
  [[ -n $SSH_CONNECTION ]]  && envs+="R"
  [[ -n $VIRTUAL_ENV ]] && envs+="P"
  [[ -n $envs ]] && echo " %F{green}[%f$envs%F{green}]%f"
}

function p_right {
  local rtside
  [[ -n $SSH_CLIENT ]] && rtside+="%F{cyan}%n@%m%f |"
  echo "$rtside %F{green}$(date "+%F %H:%M:%S")%f"
}

# reset prompt on vi-mode/insert mode change
function zle-line-init zle-keymap-select {
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

PROMPT='
$(anchor)%f $(p_colored_path)$(p_envs)$(p_vcs)
$(p_arrow) '

RPROMPT='$(p_right)'
