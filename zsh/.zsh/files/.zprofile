echo "zprofile sourced"

if [[ "$OSTYPE" == darwin* ]]; then
    export BROWSER='open'
fi

echo "Storage:"
df -h | grep "^/dev/"

if [ -x /usr/games/cowsay -a -x /usr/games/fortune ]; then
    fortune | cowsay
fi

if [[ $( date +%A ) != "Friday" ]]; then echo "Its not Friday :("; else echo "Yea Friday!"; fi
