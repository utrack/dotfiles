;;; ~/.dotfiles/doomemacs/.doom.d/+org.el -*- lexical-binding: t; -*-

;;; ~ Keybindings
;;;; ~ search on headlines via SPC-m-/
(map! (:localleader
        (:after evil-org
          :map evil-org-mode-map
          "/" #'counsel-org-goto)))


;;; ~ Settings (after org enabled
(after! org

;;;; ~ default paths
  (setq-default
   +org-directory (expand-file-name "~/org")
   org-agenda-files (list org-directory)
   org-default-notes-file (expand-file-name "inbox.org" org-directory)
   +org-capture-todo-file "inbox.org")

;;;; ~ visual
;;;;; ~ oneshot tweaks
  (setq-default
   org-ellipsis " ▼ " ;; set collapsed heading' symbols
   org-imenu-depth 6)
;;;;; ~ enable auto-fill-mode
  (add-hook 'org-mode-hook 'auto-fill-mode)
;;;;; ~ strikethrough DONE headlines - end setq-default
  (setq-default
   org-fontify-done-headline t)

  (custom-set-faces
   '(org-done ((t (
                   :weight bold
                   :strike-through t))))
   '(org-headline-done
     ((((class color) (min-colors 16) (background dark))
       (:strike-through t)))))




;;;; ~ agenda and TODO
  (setq-default
   org-agenda-persistent-filter t
   org-agenda-sticky t

   org-todo-keywords '((sequence "TODO(t)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELED(c)"))

   ;; use a bit better looking colors for todo faces
   org-todo-keyword-faces '(("TODO" . (:foreground "OrangeRed" :weight bold))
                            ("SOMEDAY" . (:foreground "GoldenRod" :weight bold))
                            ("DONE" . (:foreground "LimeGreen" :weight bold))
                            ("CANCELED" . (:foreground "gray" :weight bold)))

   ;; misc todo settings
   org-enforce-todo-dependencies t
   org-use-fast-todo-selection t
   org-fast-tag-selection-single-key nil

   ;; force me to write a note about the task when marking it done
   org-log-done 'note
   org-log-into-drawer t
   org-log-state-notes-insert-after-drawers nil

   ;; also log when items are rescheduled and refiled
   org-log-reschedule 'time
   org-log-refile     'time

   ;; prepend the filename for each org target
   org-refile-use-outline-path 'file

   org-return-follows-link t

   ;; agenda visibility
   org-agenda-span (quote fortnight)
   org-agenda-start-on-weekday nil
   org-agenda-todo-ignore-deadlines (quote all)
   org-agenda-todo-ignore-scheduled (quote all)
   org-deadline-warning-days 7
   org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)

   ;; track habits
   org-habit-show-all-today t
   org-habit-show-habits nil ;; don't show habits in agenda by default
   ;; TODO bind a button to show habits in agenda
   )

;;;; ~ automatically apply syntax highlighting
  (setq-default
   org-src-fontify-natively t
   org-src-tab-acts-natively t)

;;;; ~ misc
;;;;; ~ refiling limits
  (setq-default
   org-refile-targets '((nil :maxlevel . 5)
                        (org-agenda-files :maxlevel . 5)))

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
  (setq-default +org-dir org-directory
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
