;;; ~/.dotfiles/doomemacs/.doom.d/+org-journal.el -*- lexical-binding: t; -*-

(after! org-journal
  (setq org-journal-dir "~/Dropbox/org/journal/2020/"
        org-journal-file-type 'monthly
        org-journal-file-format "%Y-%m.orgj"
        org-journal-date-format "%e %b %Y (%A)"
        org-journal-time-format "%R ")
  (add-to-list 'auto-mode-alist '("\\.orgj" . org-journal-mode))
  (map!
   :en "C-c C-j"   #'org-journal-new-entry
   )
  )
