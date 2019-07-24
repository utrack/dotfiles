;;; ~/.dotfiles/doomemacs/.doom.d/+org.el -*- lexical-binding: t; -*-

;;; ~ Keybindings
;;;; ~ search on headlines via SPC-m-/
(map! (:localleader
        (:after evil-org
          :map evil-org-mode-map
          "/" #'counsel-org-goto)))


;;; ~ Settings (after org enabled
(after! org


;;;; ~ set default org-file path
(setq +org-directory (expand-file-name "~/org")
   org-agenda-files (list org-directory))

;;;; ~ misc
(setq org-ellipsis " ▼ ") ;; set collapsed heading' symbols

;;;;; ~ refiling limits
(setq
   org-refile-targets '((nil :maxlevel . 5)
                        (org-agenda-files :maxlevel . 5)))

;;;;; ~ set default filename for notes
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))

;;;; ~ display

;;;;; ~ strikethrough DONE headlines
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (
                 :weight bold
                 :strike-through t))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:strike-through t)))))


;;; ~ Templates
;;;; ~ aux funcs
(defun utrack/is-url (link)
  (let ((url  "\\(http[s]?://\\|www\\.\\)"))
    (string-match url link)
    )
)

  (defun utrack/clipboard-as-org-link (title)
    "If there's a URL on the clipboard, return it as an org-mode
link in the form of [[url][title]], else concat url title"
    (let ((link (substring-no-properties (x-get-selection 'CLIPBOARD))))
        (if (utrack/is-url link)
              (concat "[[" link "][" title "]]")
              (concat link " " title)
              )))
;;;; ~ definitions
  :config
  (setq +org-dir org-directory
        org-capture-templates
;;;;; ~ code task
        '(("c" "Code Task" entry (file+headline org-default-notes-file "Coding Tasks")
           "* TODO %?\n  Entered on: %U - %a\n")

;;;;; ~ task
          ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO [#B] %?\n  Entered on: %U\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))")

;;;;; ~ context task (captures filename)
          ("x" "Context Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO [#B] %?\n  Entered on: %U\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a")

;;;;; ~ reading list (captures x buffer or link in it)
          ("r" "Reading List" entry (file+headline org-default-notes-file "Reading")
           "* [ ] %(utrack/clipboard-as-org-link \"%?\")\n  Entered on: %U\n")

;;;;; ~ note
          ("n" "Note" entry (file+olp+datetree org-default-notes-file)
           "* %?\n\n")))


;;; ~ end (after org
);; after org
