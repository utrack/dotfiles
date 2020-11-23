;;; ~/.dotfiles/doomemacs/.doom.d/+company.el -*- lexical-binding: t; -*-

;;; ~ company
(after! company-box
  (setq company-box-max-candidates 7))

(after! company
  (setq-default company-tooltip-limit 7
                company-tooltip-minimum-width 80
                company-tooltip-minimum 3
                company-occurrence-weight-function (quote company-occurrence-prefer-closest-above)
                company-search-regexp-function (quote company-search-words-regexp)
                ))
