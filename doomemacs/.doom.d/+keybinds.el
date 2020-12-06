;;; ~/.dotfiles/doomemacs/.doom.d/+keybinds.el -*- lexical-binding: t; -*-

;;; poporg - popup comment editing
(map! :leader
      :desc "poporg"       "9"  #'poporg-dwim
      )

;;; workspaces
(map! :leader
      (:prefix "TAB"
        :desc "Rename workspace"       "r"  #'+workspace/rename)
      )
;;; global orgmode
(map! :leader
      (:prefix "n"
        :desc "Browse mode notes"    "m" #'+brett/find-notes-for-major-mode
        :desc "Browse project notes" "P" #'+brett/find-notes-for-project
        :desc "Capture project TODO" "p" #'+utrack/org-capture-project-todo
        :desc "Capture project note" "q" #'+utrack/org-capture-project-note)
      :desc "Capture note"       "4"  #'org-capture
      )

;;; grep project SPC-/
(map! :leader
        :desc "Search project" "/" #'+default/search-project)

;;; better mnemonics
(map! :leader
      (:prefix "b"
        :desc "Delete buffer" "d" #'kill-current-buffer)
      (:prefix "w"
        :desc "Delete window" "d" #'+workspace/close-window-or-workspace)
      )

;;; navigation
;;;; imenu for file
(map! :leader
      :desc "Imenu"       "3"  #'imenu
      )
;;;; windows
(map!
 :after evil
   :map general-override-mode-map
   :en "C-h"   #'evil-window-left
   :en "C-j"   #'evil-window-down
   :en "C-k"   #'evil-window-up
   :en "C-l"   #'evil-window-right)

;;;; treemacs
;; Allow ~C-h~ and ~C-l~ to switch buffers
(map!
 (:after treemacs-evil
   (:map evil-treemacs-state-map
     "C-h" #'evil-window-left
     "C-l" #'evil-window-right)))

;;; bindings - faster helpdisplay
(after! which-key
  (setq which-key-idle-delay 0.2
        which-key-idle-secondary-delay 0.01
        which-key-sort-order 'which-key-key-order-alpha))

;;;; toggles
(map!
 :after undo-tree
 :leader
 "tu" #'undo-tree-visualize
 )
;;;; git-links
(map! :leader
      :prefix "p"
        :desc "Copy Git link"       "l"  #'git-link)

;;;; local functions
(defun +utrack/org-capture-project-todo ()
  (interactive)
  (org-capture nil "pt")
  )

(defun +utrack/org-capture-project-note ()
  (interactive)
  (org-capture nil "pn")
  )
