;;; ~/.dotfiles/doomemacs/.doom.d/+lsp.el -*- lexical-binding: t; -*-

;; remap find-refs to lsp-ui-peek
(define-key! go-mode-map
[remap +lookup/definition] #'lsp-ui-peek-find-definitions
[remap +lookup/references] #'lsp-ui-peek-find-references)

(map!
 :after lsp
 :map go-mode-map
 :en "C-'" #'lsp-ui-imenu
 )

(map!
 :after lsp
 (:localleader
        :map go-mode-map
        "g" #'lsp-ui-peek-find-definitions
        "d" #'lsp-ui-peek-find-references
        )

      )
