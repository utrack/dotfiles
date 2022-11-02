let newkeybindings = ($env.config.keybindings | prepend {
      name: fuzzy_history
      modifier: control
      keycode: char_r
      mode: emacs
      event: {
        send: executehostcommand
        cmd: "commandline (history | each { |it| $it.command } | uniq | reverse | str collect (char -i 0) | fzf --read0 --layout=reverse --height=40% -q (commandline) | decode utf-8 | str trim)"
      }
    })

let-env config = ($env.config | upsert keybindings $newkeybindings)
