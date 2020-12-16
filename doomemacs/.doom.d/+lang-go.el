;;; ~/.dotfiles/doomemacs/.doom.d/+lang-go.el -*- lexical-binding: t; -*-

(after! lsp
  (lsp-register-custom-settings
   '(("gopls.completionBudget" "200ms" nil)
     ("gopls.allExperiments" t t)
     ("gopls.semanticTokens" t t)
     ("gopls.staticcheck" nil t)
     ("gopls.matcher" "Fuzzy" nil) )))
;; hack for https://github.com/hlissner/doom-emacs/issues/4201
(after! go-mode
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'gofmt nil 'make-it-local))))

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
