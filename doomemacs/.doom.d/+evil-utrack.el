;;; ~/.dotfiles/doomemacs/.doom.d/+evil-utrack.el -*- lexical-binding: t; -*-

;;; ~ Marks
;; re-render marks on mark deletion
(advice-add 'evil-delete-marks :after
              (lambda (&optional marks arg)
                (evil-visual-mark-render)))
