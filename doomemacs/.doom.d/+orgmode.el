(after! org

;; required by ejira, still nice to have
(setq org-id-track-globally t)

;; save org buffers when idle for 180s
(run-with-idle-timer 180 t (lambda ()
                             (org-save-all-org-buffers)))

(add-hook 'org-mode-hook 'auto-fill-mode)

(setq
 org-src-fontify-natively t
 org-src-tab-acts-natively t
 org-confirm-babel-evaluate nil
 org-return-follows-link t
 org-enforce-todo-dependencies t

 org-hierarchical-todo-statistics nil

 org-refile-targets '((nil :maxlevel . 3)
                      (org-agenda-files :maxlevel . 3))

 org-directory (expand-file-name "~/Dropbox/org-current")
 +org-directory (expand-file-name "~/Dropbox/org-current")
 +org-dir org-directory
 deft-directory org-directory
 org-agenda-files (directory-files-recursively "~/Dropbox/org-current" "org$")
 org-default-notes-file (expand-file-name "~/Dropbox/org-current/refile.org")

 org-todo-keywords '((sequence "TODO(t)" "TODAY(n)" "|" "DONE(d)" "CNCL(c)")
                     (sequence "PROJECT(p)" "|" "DONE")
                     (sequence "EPIC(e)" "|" "EFIN")
                     (sequence "WAIT(w)" "|")
                     (sequence "DELEGATED(g)" "|" "THROWN(h)"))
 org-todo-keyword-faces '(;; next
                          ("TODAY" . (:foreground "goldenrod1" :weight bold))
                          ("TODO" . (:foreground "OrangeRed"))
                          ("DONE" . (:foreground "LimeGreen"))
                          ("CNCL" . (:foreground "gray"))
                          ("WAIT" . (:foreground "PowderBlue" :weight bold))
                          ("DELEGATED" . (:foreground "SlateGray"))
                          ("EXPAND" . (:foreground "LightGoldenRod")))
 )

(setq org-modules (quote (org-habit org-id)))

(map!
 (:after evil-org
   :map evil-org-mode-map
   :i "<M-return>"   #'org-ctrl-c-ret
   :i "<M-l>"   #'org-demote-subtree
   :i "<M-h>"   #'org-promote-subtree
   :i "<M-k>"   #'org-move-subtree-up
   :i "<M-j>"   #'org-move-subtree-down

   :i "<M-j>"   #'org-next-visible-heading
   :i "<M-k>"   #'org-previous-visible-heading

   :n "J"     #'org-next-visible-heading
   :n "K"     #'org-previous-visible-heading
   :n "H"     #'org-shiftleft
   :n "L"     #'org-shiftright

   )
 )

(map! :localleader
      :after evil-org
      :map evil-org-mode-map
      "/" #'counsel-org-goto
      )

;; make spc-n-/ consistent with most localleader-/ bindings
(map! :leader
      :prefix "n"
      :desc "Jump to clocked entry" "g" #'org-clock-goto
      :desc "Search org-dir" "/" #'+default/org-notes-search)
(map! :leader
      :desc "Capture" "x" #'org-capture
      :desc "Scratch buffer" "X" #'doom/open-scratch-buffer)

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun ha/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return-indent)
    (cond
     ;; Open links like usual
     ((eq 'link (car (org-element-context)))
      (org-return-indent))
     ;; in a list
     ((org-in-item-p)
      ;; if it's non-empty *item* then we insert another item below it,
      ;; if it's an empty *item* then we change it to indented line,
      ;; if it's an empty indented line - insert double newlines below
      (if (org-element-property :contents-begin (org-element-context))
          ;; true - non-empty item, empty string
          (if (not (current-line-empty-p))
              (+org/insert-item-below 1)
            ;; empty line
            (delete-region (line-beginning-position) (line-end-position))
            (insert "\n\n")
            )
        ;; empty item
        (delete-region (line-beginning-position) (line-end-position))
        (delete-backward-char 1)
        (org-return-indent)
        ))
     ;; at heading
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
          (progn (org-end-of-meta-data)
                 (org-insert-heading))
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")))
     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (org-table-to-lisp)))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return-indent)))
     (t
      (org-return-indent))
     )))
(map!
 (:after evil-org
   :map evil-org-mode-map
   :i [return] #'ha/org-return
   :i "RET"    #'ha/org-return

   ))

(map! (:localleader
        :after evil-org
        :map evil-org-mode-map
        "q" #'air/org-set-tags))

(defun air--org-swap-tags (tags)
  "Replace any tags on the current headline with TAGS.

The assumption is that TAGS will be a string conforming to Org Mode's
tag format specifications, or nil to remove all tags."
  (let ((old-tags (org-get-tags-string))
        (tags (if tags
                  (concat " " tags)
                "")))
    (save-excursion
      (beginning-of-line)
      (re-search-forward
       (concat "[ \t]*" (regexp-quote old-tags) "[ \t]*$")
       (line-end-position) t)
      (replace-match tags)
      (org-set-tags t))))


(defun air/org-set-tags (tag)
  "Add TAG if it is not in the list of tags, remove it otherwise.

TAG is chosen interactively from the global tags completion table."
  (interactive
   (list (let ((org-last-tags-completion-table
                (if (derived-mode-p 'org-mode)
                    (org-uniquify
                     (delq nil (append (org-get-buffer-tags)
                                       (org-global-tags-completion-table))))
                  (org-global-tags-completion-table))))
           (org-icompleting-read
            "Tag: " 'org-tags-completion-function nil nil nil
            'org-tags-history))))
  (let* ((cur-list (org-get-tags))
         (new-tags (mapconcat 'identity
                              (if (member tag cur-list)
                                  (delete tag cur-list)
                                (append cur-list (list tag)))
                              ":"))
         (new (if (> (length new-tags) 1) (concat " :" new-tags ":")
                nil)))
    (air--org-swap-tags new)))

(setq
 org-ellipsis " ▼ "
 org-superstar-headline-bullets-list (quote ("◉" "✿" "★" "•"))
 org-startup-folded t
 org-imenu-depth 6)

(let* ((variable-tuple (cond ((x-list-fonts "Open Sans") '(:font "Open Sans"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Open Sans."))))
       (headline           `(:inherit default
                             ;;:weight bold
                             )))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.1 ))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.25 :weight bold))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.25 :weight bold))))))
(add-hook 'org-mode-hook
          (lambda ()
            "Beautify Org Checkbox Symbol"
            (push '("[ ]" .  "☐") prettify-symbols-alist)
            (push '("[X]" . "☑" ) prettify-symbols-alist)
            (push '("[-]" . "❍" ) prettify-symbols-alist)
            (push '(":LOGBOOK:" . "🕘" ) prettify-symbols-alist)
            (push '(":END:" . "⇤" ) prettify-symbols-alist)
            (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
            (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
            (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
            (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
            (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
            (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
            (push '("#+begin_quote" . "↦" ) prettify-symbols-alist)
            (push '("#+end_quote" . "⇤" ) prettify-symbols-alist)
            (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
            (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
            (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
            (push '("#+end_src" . "⇤" ) prettify-symbols-alist)
            (prettify-symbols-mode +1)))

  (setq
   org-enforce-todo-dependencies t ;; children TODOs block parents by default
   org-use-fast-todo-selection t ;; hotkey C-c C-t
   org-fast-tag-selection-single-key t

   ;; force me to write a note about the task when marking it done
   org-log-done 'note
   org-log-into-drawer nil

   ;; also log time when items are rescheduled and refiled
   org-log-reschedule 'time
   org-log-refile     'time)

;; https://gitlab.com/howardabrams/spacemacs.d/-/blob/master/layers/ha-org/funcs.el#L367
;; http://howardism.org/Technical/Emacs/capturing-content.html
(defun ha/org-capture-code-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org SRC block with a language based on the current mode
and a backlink to the function and the file."
  (with-current-buffer (find-buffer-visiting f)
    (let ((org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))
          (func-name (which-function)))
      (ha/org-capture-fileref-snippet f "SRC" org-src-mode func-name))))
(defun ha/org-capture-clip-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org EXAMPLE block and a backlink to the file."
  (with-current-buffer (find-buffer-visiting f)
    (ha/org-capture-fileref-snippet f "EXAMPLE" "" nil)))
(defun ha/org-capture-fileref-snippet (f type headers func-name)
  (let* ((code-snippet
          (buffer-substring-no-properties (mark) (- (point) 1)))
         (file-name   (buffer-file-name))
         (file-base   (file-name-nondirectory file-name))
         (line-number (line-number-at-pos (region-beginning)))
         (initial-txt (if (null func-name)
                          (format "From [[file:%s::%s][%s]]:"
                                  file-name line-number file-base)
                        (format "From ~%s~ (in [[file:%s::%s][%s]]):"
                                func-name file-name line-number
                                file-base))))
    (format "
   %s

   #+BEGIN_%s %s
%s
   #+END_%s" initial-txt type headers code-snippet type)))

(setq org-capture-templates '(
                              ("i" "Inbox" entry (file+headline org-default-notes-file "Inbox")
                               "* TODO [#B] %?\t:@unsorted:\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\nEntered on: %U\n")
                              ("p" "Inbox: Personal" entry (file+headline org-default-notes-file "Personal")
                               "* TODO [#B] %?\t :@personal:\nEntered on: %U\n")

                              ("c" "cl: capture an item" item (clock) "%i\n  %?" :empty-lines 1)
                              ("h" "cl: dump immediately" plain (clock) "%i" :immediate-finish t :empty-lines 1)

                              ("s" "cl: subtask snip" entry (clock)
                               "* %?\n%(ha/org-capture-code-snippet \"%F\")" :empty-lines 1)
                              ("e" "cl: snip" plain (clock)
                               "%?\n%(ha/org-capture-code-snippet \"%F\")" :empty-lines 1)
                              ("i" "cl: new item" entry (clock)
                               "%?\nref: %a\n%i" :empty-lines 1)
                              ))

(defun utrack/notes-path-for-project ()
  ;; Open roam file "Project 'name'.org"
  (interactive)
  (let ((project-root (doom-project-name))
        (default-directory (expand-file-name "roam/" org-directory)))
    (expand-file-name (concat "Project " project-root ".org")))
  )

(defun utrack/hooks/schedule-to-today ()
  "Schedule TODAY item to today."
  (save-excursion
    (and (equal (org-get-todo-state) "TODAY")
         (org-schedule nil "today")
         (get-buffer "*Org Agenda*")
         (with-current-buffer "*Org Agenda*"
           (org-agenda-redo)))))
(add-hook 'org-after-todo-state-change-hook
          'utrack/hooks/schedule-to-today)

(defun utrack/hooks/org-mode-epic-cookie ()
  "Add counter cookie to items marked EPIC."
  (interactive)
  (if (equal (org-get-todo-state) "EPIC")
      (progn
        (end-of-line)
        (insert " [/]")
        (org-update-statistics-cookies nil))))

(add-hook 'org-after-todo-state-change-hook
          'utrack/hooks/org-mode-epic-cookie)

) ;; end after! org

(require 'org-ql)
(after! org-ql
  (map! :leader
        :prefix "n"
        :desc "Agendas" "a" #'org-ql-view)
  (defun +utrack/org-ql-show-now ()
    "Show 'Now' org-ql view."
    (interactive)
    (org-ql-view "Now"))
  (defun +utrack/org-ql-show-stuck ()
    "Show 'Stuck' org-ql view."
    (interactive)
    (org-ql-view "Projects w/o action points (stuck)"))
  (defun +utrack/org-ql-show-pick ()
    "Show 'Daily Pick' org-ql view."
    (interactive)
    (org-ql-view "Daily Picker"))
  (map! :leader
        :prefix "oa"
        :desc "Now" "n" #'+utrack/org-ql-show-now
        :desc "Pick" "p" #'+utrack/org-ql-show-pick
        :desc "Stuck" "s" #'+utrack/org-ql-show-stuck
        :desc "Agendas" "a" #'org-ql-view)

  (setq org-ql-views '(
                       ("Daily Picker"
                        :buffers-files org-agenda-files
                        :query
                        (and (not (done))
                             (not (habit))
                             (not (property "TYPE" "ejira-epic"))
                             (not (property "BLOCKED" "t"))
                             (or ;; show only those jira tix assigned to me
                              (not (property "TYPE" "ejira-issue"))
                              (tags "ejira_assigned"))

                             (or
                              (scheduled :to 5)
                              (deadline :to 5)
                              (todo "TODAY")))
                        :sort date
                        :narrow nil
                        :super-groups
                        (
                         (:name "Today"
                          :time-grid t
                          :and
                          (:todo "TODAY" :scheduled today)
                          :order 1)
                         (:name "Overdue TODAYs"
                          :and
                          (:todo "TODAY" :scheduled past)
                          :and
                          (:todo "TODAY" :deadline past)
                          :order 10)
                         (:name "Overdue"
                          :scheduled past
                          :deadline past
                          :order 11)
                         (:name "Candidates"
                          :scheduled today
                          :deadline today
                          :order 12)
                         (:name "Upcoming"
                          :scheduled future
                          :deadline future
                          :transformer (--> it
                                            (propertize it 'face '(:foreground "MistyRose4")))
                          :order 13))
                        :title "Daily Picker")

                       ("Projects w/o action points (stuck)"
                        :title "Stuck projects"
                        :buffers-files org-agenda-files
                        :query
                        (and (todo)
                             (not (done))
                             (not (scheduled))
                             (or (descendants (todo))
                                 (descendants (done)))
                             (not (descendants (scheduled :from today))))
                        :sort date
                        :narrow t
                        :super-groups ((:auto-category t)))
                       ("Unscheduled TODOs"
                        :title "Unscheduled"
                        :buffers-files org-agenda-files
                        :query
                        (and
                         (todo)
                         (not (done))
                         (not (scheduled))
                         (not (deadline))
                         (not (property "BLOCKED" "t"))
                         (not (todo "TODAY" "EPIC" "PROJECT")))
                        :super-groups
                        (
                         (:name "Jira assigned" :tag "ejira_assigned")
                         (:name "Waiting" :todo ("WAIT"))
                         (:auto-parent)))
                       ("Now"
                        :title "Now"
                        :buffers-files org-agenda-files
                        :query
                        (or
                         (closed :on today) ;; log

                         (and
                          (todo "EPIC" "PROJECT") ;; touched this today
                          (or (ts :on today)
                              (descendants (ts :on today))))

                         (and
                          (not (done))

                          ;; show only those jira tix assigned to me
                          (not (property "TYPE" "ejira-epic"))
                          (or
                           (not (property "TYPE" "ejira-issue"))
                           (tags "ejira_assigned"))

                          (or (habit)
                              (scheduled :on today)
                              (ts-active :on today))))
                        :super-groups (
                                       (:name "Done so far"
                                        :todo ("DONE" "EFIN" "CNCL" "THROWN"))
                                       (:habit t)
                                       (:name "Epics touched today"
                                        :todo ("EPIC" "PROJECT"))
                                       (:auto-parent t)
                                       )
                        )

                       )))
