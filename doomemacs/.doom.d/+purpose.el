;;; ~/.dotfiles/doomemacs/.doom.d/+purpose.el -*- lexical-binding: t; -*-

(purpose-x-magit-single-on)
(purpose-x-golden-ratio-setup)
(setq purpose-use-default-configuration t)
(purpose-x-kill-setup)
(purpose-compile-user-configuration)

(setq-default magit-post-display-buffer-hook (quote (utrack/purpose-dedicate-window)))

(defun utrack/purpose-dedicate-window ()
  (interactive)
  (purpose-set-window-purpose-dedicated-p nil t))
