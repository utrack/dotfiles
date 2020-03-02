;;; ~/.dotfiles/doomemacs/.doom.d/+lang-go.el -*- lexical-binding: t; -*-

;; (add-hook 'go-mode-hook #'lsp)
(after! lsp
(setq lsp-gopls-use-placeholders t)
(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ))
)
