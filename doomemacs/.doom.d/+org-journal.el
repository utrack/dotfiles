;;; ~/.dotfiles/doomemacs/.doom.d/+org-journal.el -*- lexical-binding: t; -*-

(setq org-journal-dir "~/Dropbox/org-current/roam"
      org-journal-file-type 'monthly
      org-journal-file-format "%Y-%m.org"
      org-journal-date-format "%e %b %Y (%A)"
      org-journal-time-format "%R ")

(use-package org-roam
  :hook
  ((org-mode . org-roam-mode)
   (after-init . org-roam--build-cache-async) ;; optional!
   )
  )
(after! org-roam
  (setq org-roam-directory "~/Dropbox/org-current/roam")
  (map!
   :leader
   (:prefix "n"

     :desc "Journal entry" "j"   #'org-journal-new-entry
     :desc "Roam backlinks" "r"   #'org-roam
     :desc "Roam today" "T"   #'org-roam-today
     :desc "Roam find" "f"   #'org-roam-find-file
     :desc "Roam insert" "i"   #'org-roam-insert
     :desc "Roam graph" "G"   #'org-roam-show-graph
     ))
  (map!
   :ni "C-c r" #'org-roam-insert)
  )
