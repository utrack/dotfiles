;;; ~/.dotfiles/doomemacs/.doom.d/+org-journal.el -*- lexical-binding: t; -*-

(setq org-journal-dir "~/Dropbox/org-current/roam"
      org-journal-file-type 'daily
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-date-format "%e %b %Y (%A)"
      org-journal-time-format "%R ")
(map!
 :en "C-c n j"   #'org-journal-new-entry
 )

(after! org
  (use-package org-roam)

  (add-hook 'org-mode-hook #'org-roam-mode)
  ;(add-hook 'after-init-hook #'org-roam--build-cache-async)
  (setq org-roam-directory "~/Dropbox/org-current/roam")

  (map!
   "C-c n l" #'org-roam
   "C-c n t" #'org-roam-today
   "C-c n f" #'org-roam-find-file
   "C-c n i" #'org-roam-insert
   "C-c n g" #'org-roam-show-graph
   )
  )
