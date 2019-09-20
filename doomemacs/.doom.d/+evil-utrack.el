;;; ~/.dotfiles/doomemacs/.doom.d/+evil-utrack.el -*- lexical-binding: t; -*-

;;; ~ Marks
;; re-render marks on mark deletion
(advice-add 'evil-delete-marks :after
            (lambda (&optional marks arg)
              (evil-visual-mark-render)))

(after! evil-snipe
        (evil-snipe-mode +1)
        (evil-snipe-override-mode +1)
        )

(after! evil
        (setq-default
          evil-goggles-enable-change t
          evil-goggles-enable-delete t
          evil-visual-mark-mode t
          )
        )
