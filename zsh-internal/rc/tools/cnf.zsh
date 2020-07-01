#!/usr/bin/env zsh
set -euo pipefail

# command not found
# Debian/Ubuntu
if [[ -s '/etc/zsh_command_not_found' ]]; then
  source '/etc/zsh_command_not_found'
fi
# Arch
if [[ -s '/etc/profile.d/cnf.sh' ]]; then
  . /etc/profile.d/cnf.sh
fi

