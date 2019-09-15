;;; ~/.dotfiles/doomemacs/.doom.d/+evil-utrack.el -*- lexical-binding: t; -*-

;;; ~ Marks
;; re-render marks on mark deletion
(advice-add 'evil-delete-marks :after
            (lambda ()
              (evil-visual-mark-render)))

(after! evil
  (setq-default
   evil-goggles-enable-change t
   evil-goggles-enable-delete t
   evil-visual-mark-mode t
   )
  )
