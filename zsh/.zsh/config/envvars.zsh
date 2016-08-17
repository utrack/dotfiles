# Compilation flags
export ARCHFLAGS="-arch x86_64"

# path to configs
export XDG_CONFIG_HOME="$HOME/.config"

export VISUAL=vim
export EDITOR="$VISUAL"

# Java bspwm workarounds
export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dawt.useSystemAAFontSettings=true' 
export JAVA_FONTS=/usr/share/fonts/TTF
export _JAVA_AWT_WM_NONREPARENTING=true
