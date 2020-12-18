;;; ../.dotfiles/doomemacs/.doom.d/my/doom-nano.el -*- lexical-binding: t; -*-

;; This is my configuration for nano-emacs' specific things that (probably) could
;; move to nano's own module.

;;; Code:
(require 'disp-table)
(require 'nano-theme-light)
(require 'nano-theme-dark)
(require 'nano-layout)
(require 'nano-faces)
(require 'nano-theme)

(defun +utrack/nano-customizations ()
  (set-face 'italic                                     'nano-face-salient)
  (set-face 'org-list-dt                                'nano-face-salient)
  (set-face 'org-scheduled-previously                   'nano-face-salient)
  (set-face 'org-scheduled-today                        'nano-face-salient)

  (after! org-superstar
    ;; make list dots red in org-mode
    (set-face-attribute 'org-superstar-item nil
                        :foreground "OrangeRed1"))
  )

(defun nano-theme-dark ()
  "Enable dark Nano theme and customizations."
  (interactive)
  (nano-theme-set-dark)
  (setq nano-color-faded      "#677691") ;; make faded a bit lighter, my vision isnt so good ;(
  (+utrack/nano-customizations)
  (nano-faces)
  (nano-theme)
  )
(nano-theme-dark) ;; hack to apply customizations

(defun nano-theme-light ()
  "Enable dark Nano theme and customizations."
  (interactive)
  (nano-theme-set-light)
  (+utrack/nano-customizations)
  (nano-faces)
  (nano-theme)
  )

(provide 'doom-nano)
