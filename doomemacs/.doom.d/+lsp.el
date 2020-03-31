;;; ~/.dotfiles/doomemacs/.doom.d/+lsp.el -*- lexical-binding: t; -*-


(after! lsp
  (setq
   ;; TODO to company
   company-lsp-cache-candidates (quote auto)

   lsp-ui-doc-enable t
   lsp-ui-doc-max-height 8
   lsp-ui-doc-max-width 40
   lsp-ui-doc-use-childframe t
   lsp-ui-sideline-ignore-duplicate t
   lsp-ui-doc-delay 0.15
   lsp-ui-doc-include-signature t
   )
  )
