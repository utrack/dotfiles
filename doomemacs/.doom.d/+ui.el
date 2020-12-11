;;; ~/.dotfiles/doomemacs/.doom.d/+ui.el -*- lexical-binding: t; -*-

;; enable beacon-mode everywhere
(beacon-mode 1)


;;(setq doom-font (font-spec :family "Fira Code" :size 17))
;;(setq doom-theme 'doom-zenburn)


;;; ~ small tweaks
;; compat with tiling managers
(setq frame-resize-pixelwise t)

;; highlight trailing whitespace
(setq show-trailing-whitespace t)

;; show eldoc immediately
(setq eldoc-idle-delay 0.01)

;; relnumbers
(setq display-line-numbers-type 'relative)

;;;; ~ modeline
(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  ;; (setq doom-modeline-bar-width 3)
  )

;;;; ~ global eldoc
(global-eldoc-mode 1)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)
(add-hook 'org-mode-hook 'eldoc-mode)

;;; ~ themes
(after! doom-themes
  (setq
   doom-themes-enable-bold t
   doom-themes-enable-italic t))

;;; ~ prefer vertical splits
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
(with-eval-after-load "window"
  (defcustom split-window-below nil
    "If non-nil, vertical splits produce new windows below."
    :group 'windows
    :type 'boolean)

  (defcustom split-window-right nil
    "If non-nil, horizontal splits produce new windows to the right."
    :group 'windows
    :type 'boolean)

  (fmakunbound #'split-window-sensibly)

  (defun split-window-sensibly
      (&optional window)
    (setq window (or window (selected-window)))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (split-window window nil (if split-window-right 'left  'right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (split-window window nil (if split-window-below 'above 'below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding the
             ;; value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil (if split-window-right
                                              'left
                                            'right))))))))

(setq-default split-height-threshold  4
              split-width-threshold   160) ; the reasonable limit for horizontal splits
