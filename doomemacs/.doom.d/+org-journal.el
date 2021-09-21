;;; ~/.dotfiles/doomemacs/.doom.d/+org-journal.el -*- lexical-binding: t; -*-


(after! org-roam
  (setq org-roam-directory "~/Dropbox/org-current/roam"
        +org-roam-open-buffer-on-find-file nil)


  (map!
   :map org-mode-map
   :ni "C-c r" #'org-roam-node-insert)

  (map!
   :map org-mode-map
   :leader
   (:prefix "n"
     :desc "Roam insert" "i"   #'org-roam-node-insert))

(map!
 :leader
 (:prefix "n"
   :desc "Journal entry" "j"   #'org-journal-new-entry
   ;;:desc "Roam backlinks" "r"   #'org-roam
   ;;:desc "Roam today" "r"   #'org-roam-dailies-find-today
   :desc "Roam find" "f"   #'org-roam-node-find
   :desc "Find notes" "F"   #'+default/find-in-notes
   :desc "Roam graph" "G"   #'org-roam-show-graph
   ))
(setq
org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%H:%M>: %?"
       :if-new (file+head "daily-%<%Y-%m-%d>.org" "#+title: daily/%<%Y-%m-%d>\n"))
      ))

  )
