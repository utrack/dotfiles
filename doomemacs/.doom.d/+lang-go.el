;;; ~/.dotfiles/doomemacs/.doom.d/+lang-go.el -*- lexical-binding: t; -*-

(after! lsp
  (setq lsp-gopls-use-placeholders t)
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ))
  )


(map!
 :after lsp
 :map go-mode-map
 :en "C-'" #'lsp-ui-imenu
 )

(map!
 :map go-mode-map
 :after go-mode
 :localleader
 "D" #'lsp-ui-peek-find-definitions
 "G" #'lsp-ui-peek-find-references
 "d" #'lsp-find-definition
 "g" #'lsp-find-references
 "i" #'lsp-find-implementation
 )
