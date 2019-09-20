;;; ~/.dotfiles/doomemacs/.doom.d/+company.el -*- lexical-binding: t; -*-

;;; ~ company
(def-package! company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))

(after! company-box
  (setq company-box-max-candidates 7))

(after! company
  (setq-default company-tooltip-limit 7
                company-tooltip-minimum-width 80
                company-tooltip-minimum 3
                company-backends
                '(company-capf company-dabbrev company-files company-yasnippet)
                company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
                company-occurrence-weight-function (quote company-occurrence-prefer-closest-above)
                company-search-regexp-function (quote company-search-words-regexp)
                ))

;;; ~ ivy
(setq +ivy-project-search-engines '(rg))

(def-package! counsel-tramp
  :commands (counsel-tramp))
