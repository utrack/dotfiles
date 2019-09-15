;;; ~/.dotfiles/doomemacs/.doom.d/+ivy.el -*- lexical-binding: t; -*-

(after! ivy
  (setq-default
   ivy-fixed-height-minibuffer t
   ivy-height 15
   ivy-magic-slash-non-match-action nil
   ivy-mode t
   ivy-on-del-error-function nil
   ivy-rich-mode t
   ivy-use-selectable-prompt t
   ivy-virtual-abbreviate (quote full)
   ivy-wrap t
   )

  )
