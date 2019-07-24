;;; ~/.dotfiles/doomemacs/.doom.d/+magit.el -*- lexical-binding: t; -*-

(after! magit
  (setq magit-display-buffer-function #'magit-display-buffer-traditional))
(add-to-list 'display-buffer-alist
             `(,(rx bos "*magit:")
               (display-buffer-reuse-window
                display-buffer-below-selected)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))
