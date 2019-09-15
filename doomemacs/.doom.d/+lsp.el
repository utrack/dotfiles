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
(after! lsp
  (setq-default
   ;; TODO to company
   company-lsp-cache-candidates (quote auto)

   lsp-ui-doc-enable t
   lsp-ui-doc-max-height 8
   lsp-ui-doc-max-width 35
   lsp-ui-doc-use-childframe t
   lsp-ui-sideline-ignore-duplicate t
   )
  )
