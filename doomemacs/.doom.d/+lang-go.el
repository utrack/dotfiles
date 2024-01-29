;;; ~/.dotfiles/doomemacs/.doom.d/+lang-go.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (lsp-register-custom-settings
   '(("gopls.completionBudget" "400ms" nil)
     ;;("gopls.ui.completion.experimentalPostfixCompletions" t t) enabled by default in v0.7.0
     ("gopls.allExperiments" t t)
     ("gopls.gofumpt" t t)
     ("gopls.semanticTokens" t t)
     ("gopls.staticcheck" nil t) ;; experimental checkers from staticcheck.io
     ;;("gopls.matcher" "Fuzzy" nil)
     ("gopls.usePlaceholders" t t)
     ("gopls.analyses.nilness" t t)
     ("gopls.analyses.shadow" t t)
     ("gopls.analyses.unusedparams" t t)
     ("gopls.analyses.fieldalignment" t t)
     ("gopls.analyses.unusedvariable" t t)
     ("gopls.analyses.unusedwrite" t t)
     ("gopls.hoverKind" "SynopsisDocumentation" nil)
     ))
  ;;(setq lsp-go-gopls-server-args "--debug=localhost:6060")
  )
;; hack for https://github.com/hlissner/doom-emacs/issues/4201
(after! go-mode
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'gofmt nil 'make-it-local))))

(map!
 :after lsp-mode
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
