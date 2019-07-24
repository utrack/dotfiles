;;; ~/.dotfiles/doomemacs/.doom.d/+lang-go.el -*- lexical-binding: t; -*-

(add-hook 'go-mode-hook #'lsp)

(map! (:localleader
          :map go-mode-map
          "g" #'lsp-find-definition))
