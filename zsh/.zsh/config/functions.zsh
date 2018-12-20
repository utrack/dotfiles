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

function watch() { while inotifywait --exclude .swp -e modify -r .; do $@; done; }

function retry() {
    local n=$1
    shift
    for i in $(seq $n); do
        "$@"
    done
}


# https://github.com/y0ssar1an/q
function qq() {
    clear

    logpath="$TMPDIR/q"
    if [[ -z "$TMPDIR" ]]; then
        logpath="/tmp/q"
    fi

    if [[ ! -f "$logpath" ]]; then
        echo 'Q LOG' > "$logpath"
    fi

    tail -100f -- "$logpath"
}
function rmqq() {
    logpath="$TMPDIR/q"
    if [[ -z "$TMPDIR" ]]; then
        logpath="/tmp/q"
    fi
    if [[ -f "$logpath" ]]; then
        rm "$logpath"
    fi
    qq
}
