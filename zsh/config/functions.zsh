function lt() { ls -ltrsa "$@" | tail; }
function psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto; }
function search() { find . -iname "*$@*" | less; }
function genpasswd {
local l=12 # default password lenght
if [ "$#" != "0" -a "$1" -gt 0 ]
then
  l=$1
fi
tr -dc A-Za-z0-9_ < /dev/urandom | head -c ${l} | xargs
}
