;;; ~/.dotfiles/doomemacs/.doom.d/+org-journal.el -*- lexical-binding: t; -*-

(setq org-journal-dir "~/Dropbox/org-current/roam"
      org-journal-file-type 'monthly
      org-journal-file-format "%Y-%m.org"
      org-journal-date-format "%e %b %Y (%A)"
      org-journal-time-format "%R ")

(after! org-roam
  (setq org-roam-directory "~/Dropbox/org-current/roam")

  (map!
   :map org-mode-map
   :ni "C-c r" #'org-roam-insert)

  (map!
   :map org-mode-map
   :leader
   (:prefix "n"
     :desc "Roam insert" "i"   #'org-roam-insert))
  )

(map!
 :leader
 (:prefix "n"
   :desc "Journal entry" "j"   #'org-journal-new-entry
   :desc "Roam backlinks" "r"   #'org-roam
   :desc "Roam today" "T"   #'org-roam-dailies-today
   :desc "Roam find" "f"   #'org-roam-find-file
   :desc "Roam graph" "G"   #'org-roam-show-graph
   ))
