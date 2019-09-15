;;; ~/.dotfiles/doomemacs/.doom.d/+org.el -*- lexical-binding: t; -*-

;;; ~ Keybindings
;;;; ~ vim-esque headings, consistent insert mode
(map!
 (:after evil-org
   :map evil-org-mode-map
   :i "<M-return>"   #'org-ctrl-c-ret

   :i "<M-l>"   #'org-demote-subtree
   :i "<M-h>"   #'org-promote-subtree
   :n "<M-k>"   #'org-move-subtree-up
   :n "<M-j>"   #'org-move-subtree-down

   :n "J"     #'org-next-visible-heading
   :n "K"     #'org-previous-visible-heading
   :n "H"     #'org-shiftleft
   :n "L"     #'org-shiftright

   )
 )
;;;; ~ search on headlines via SPC-m-/
(map! (:localleader
        (:after evil-org
          :map evil-org-mode-map
          "/" #'counsel-org-goto)))
;;;; ~ insert internal link on lleader-l
(map! (:localleader
        (:after evil-org
          :map evil-org-mode-map
          "l" #'bjm/org-insert-internal-link)))
;;;; ~ capture at point
(map! (:localleader
        (:after evil-org
          :map evil-org-mode-map
          "x" #'utrack/org-capture-at-point)))
;;; ~ Settings (after org enabled
(after! org

;;;; ~ default paths
  (setq-default
   +org-directory (expand-file-name "~/org")
   org-agenda-files (list org-directory)
   org-default-notes-file (expand-file-name "inbox.org" org-directory)
   +org-capture-todo-file "inbox.org")

;;;; ~ web of links
  (setq-default
   org-id-link-to-org-use-id t
   )
;;;; ~ visual
;;;;; ~ oneshot tweaks
  (setq-default
   org-ellipsis " ▼ " ;; set collapsed heading' symbols
   org-imenu-depth 6)
;;;;; ~ enable auto-fill-mode
  (add-hook 'org-mode-hook 'auto-fill-mode)
;;;;; ~ strikethrough DONE headlines
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
  (setq
   org-agenda-window-setup (quote reorganize-frame))
  (setq-default
   org-agenda-persistent-filter t
   org-agenda-sticky t

   org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
                       (sequence "WAITING(w)" "EXPAND(e)" "|")
                       (sequence "DELEGATED(g)" "|" "THROWN(x)")
                       )

   ;; use a bit better looking colors for todo faces
   org-todo-keyword-faces '(
                            ;; next
                            ("TODO" . (:foreground "OrangeRed" :weight bold))
                            ("DONE" . (:foreground "LimeGreen"))
                            ("CANCELLED" . (:foreground "gray"))

                            ("WAITING" . (:foreground "PowderBlue" :weight bold))

                            ("DELEGATED" . (:foreground "SlateGray"))
                            ;; thrown

                            ("PROJ" . (:foreground "DarkTurquoise"))
                            ("EXPAND" . (:foreground "LightGoldenRod"))
                            )


   ;; misc todo settings
   org-enforce-todo-dependencies t
   org-use-fast-todo-selection t
   org-fast-tag-selection-single-key nil
   org-agenda-dim-blocked-tasks t

   ;;org-adapt-indentation nil


   ;; force me to write a note about the task when marking it done
   org-log-done 'note
   org-log-into-drawer t

   ;; also log when items are rescheduled and refiled
   org-log-reschedule 'time
   org-log-refile     'time

   ;; prepend the filename for each org target
   ;; org-refile-use-outline-path 'file
   ;; buggy w/ doomemacs since it has own formatter (TODO find it out)

   org-return-follows-link t

   ;; agenda visibility
   org-agenda-span (quote fortnight)
   org-agenda-start-on-weekday nil
   org-agenda-todo-ignore-deadlines (quote all)
   org-agenda-todo-ignore-scheduled (quote all)
   org-deadline-warning-days 7
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
   org-agenda-inhibit-startup nil
   org-agenda-sorting-strategy (quote
                                ((agenda deadline-up priority-down)
                                 (todo priority-down category-keep)
                                 (tags priority-down category-keep)
                                 (search category-keep)))

   org-modules (quote
                (org-bibtex org-habit org-id org-notify org-panel org-registry))

   ;; track habits
   ;;org-habit-show-all-today t
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
                        (org-agenda-files :maxlevel . 3)))

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
                   "* TODO %?\nEntered on: %U - %a\n")

;;;;; ~ task
                  ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
                   "* TODO [#B] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n  Entered on: %U\n")

;;;;; ~ context task (captures filename)
                  ("x" "Context Task" entry (file+headline org-default-notes-file "Tasks")
                   "* TODO [#B] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n  Entered on: %U\n%a")

;;;;; ~ context task for project - captures to project notes
                  ("pt" "Context Project Task" entry (file+headline utrack/notes-path-for-project "Tasks")
                   "* TODO [#B] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n  Entered on: %U\n%a")
;;;;; ~ context note for project - captures to project notes
                  ("pn" "Context Project Note" entry (file+headline utrack/notes-path-for-project "Notes")
                   "* %?\n  Entered on: %U\n%a")

;;;;; ~ reading list (captures x buffer or link in it)
                  ("r" "Reading List" entry (file+headline org-default-notes-file "Reading")
                   "* [ ] %(utrack/clipboard-as-org-link \"%?\")\n  Entered on: %U\n")

;;;;; ~ note
                  ("n" "Note" entry (file+olp+datetree org-default-notes-file)
                   "* %?\n\n")))


;;; ~ various functions used above

;;;; ~ org-insert-internal-link
  ;; use ivy to insert a link to a heading in the current document
  (defun bjm/org-insert-internal-link ()
    "Use ivy to insert a link to a heading in the current `org-mode' document."
    (interactive)
    (let ((settings (cdr (assq major-mode counsel-outline-settings))))
      (let ((cands (counsel-outline-candidates settings)))
        (ivy-read "Heading: " cands
                  :action 'bjm/org-insert-internal-link-action))))

  (defun bjm/org-insert-internal-link-action (x)
    "Insert link for `bjm/org-insert-internal-link'"
    ;; go to heading
    (save-excursion
      (goto-char (cdr x))
      ;; store link
      (call-interactively 'org-store-link)
      )
    ;; return to original point and insert link
    (org-insert-last-stored-link 1)
    ;; org-insert-last-stored-link adds a newline so delete this
    (delete-backward-char 1)
    )
;;;; ~ capture-at-point
  (defun utrack/org-capture-at-point ()
    "Insert an org capture template at point."
    (interactive)
    (org-capture 0))

;;;; ~ notes for project
  (defun utrack/notes-path-for-project ()
    (interactive)
    (let ((project-root (doom-project-name))
          (default-directory (expand-file-name "projects/" org-directory)))
      (expand-file-name (concat project-root ".org")))
    )

  (defun +brett/find-notes-for-project (&optional arg)
    "Find notes for the current project"
    (interactive "P")
    (if arg
        (call-interactively #'find-file)
      (find-file (utrack/notes-path-for-project))))

;;; ~ end (after org
  );; after org
