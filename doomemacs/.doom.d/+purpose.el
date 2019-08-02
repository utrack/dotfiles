;;; ~/.dotfiles/doomemacs/.doom.d/+purpose.el -*- lexical-binding: t; -*-

;; WARN disabled in init.el

(purpose-x-magit-single-on)
(purpose-x-golden-ratio-setup)
(setq purpose-use-default-configuration t)
(purpose-x-kill-setup)
(purpose-compile-user-configuration)

(setq-default magit-post-display-buffer-hook (quote (utrack/purpose-dedicate-window)))

(defun utrack/purpose-dedicate-window ()
  (interactive)
  (purpose-set-window-purpose-dedicated-p nil t))

(map! :leader
      "tp" #'purpose-toggle-window-purpose-dedicated)
