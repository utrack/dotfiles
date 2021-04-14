;;; ~/.dotfiles/doomemacs/.doom.d/+lsp.el -*- lexical-binding: t; -*-


(after! lsp
  (setq lsp-go-use-placeholders t
        lsp-auto-configure t
        lsp-enable-on-type-formatting t
        lsp-semantic-tokens-enable t)
  (setq
   lsp-ui-sideline-show-hover t
   lsp-ui-sideline-delay 1
   lsp-ui-sideline-show-code-actions t)
  (setq
   lsp-ui-doc-enable t
   lsp-ui-doc-max-height 8
   lsp-ui-doc-max-width 40
   lsp-ui-doc-use-childframe t
   lsp-ui-sideline-ignore-duplicate t
   lsp-ui-doc-delay 0.3
   lsp-ui-doc-include-signature t
   )
  )
