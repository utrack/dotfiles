# Fixes terminal breakout after opening binary files.
# Thx http://habrahabr.ru/post/272581/
_echoti() {
    emulate -L zsh
    (( ${+terminfo[$1]} )) && echoti $1
}
term_reset() {
    emulate -L zsh
    [[ -n $TTY ]] && (( $+terminfo )) && {
        _echoti rmacs  # Отключает графический режим
        _echoti sgr0   # Убирает цвет
        _echoti cnorm  # Показывает курсор
        _echoti smkx   # Включает «keyboard transmit mode»
        echo -n $'\e[?47l' # Отключает alternate screen
        # See https://github.com/fish-shell/fish-shell/issues/2139 for smkx
    }
}
zmodload zsh/terminfo && precmd_functions+=( term_reset )

ttyctl -f
