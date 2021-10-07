;; ~/.dotfiles/doomemacs/.doom.d/+magit.el -*- lexical-binding: t; -*-

(after! magit
  (setq git-commit-summary-max-length 75)
  (setq magit-display-buffer-function #'magit-display-buffer-traditional)
  )
 (add-to-list 'display-buffer-alist
              `(,(rx bos "*magit:")
                (display-buffer-reuse-window
                 display-buffer-below-selected)
                (reusable-frames . visible)
                (side            . bottom)
                (window-height   . 0.4)))

 (map!
  :after magit
  :map magit-file-section-map
    "<return>" #'+utrack/magit-diff-visit-file-below)
 (map!
  :after magit
  :map magit-hunk-section-map
    "<return>" #'+utrack/magit-diff-visit-file-below)

 (defun +utrack/magit-diff-visit-file-below (file)
   (interactive (list (magit-file-at-point t t)))
   (magit-diff-visit-file--internal file nil #'switch-to-buffer-sibling-or-below))

 (defun display-buffer-child-or-below (buffer alist)
  (let (
     (window
       (cond
        ((window-next-sibling))
         (t
           (split-window (selected-window) nil 'below)))))
   (window--display-buffer buffer window 'window alist)))

 (defun switch-to-buffer-sibling-or-below (buffer-or-name &optional norecord)
   (interactive
    (list (read-buffer-to-switch "Switch to buffer in other window: ")))
   (let ()
     (pop-to-buffer buffer-or-name '(display-buffer-child-or-below) norecord)))
