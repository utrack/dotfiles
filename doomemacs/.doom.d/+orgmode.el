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

 org-refile-targets '((nil :maxlevel . 3)
                      (org-agenda-files :maxlevel . 3))

 org-directory (expand-file-name "~/Dropbox/org-current")
 +org-directory (expand-file-name "~/Dropbox/org-current")
 +org-dir org-directory
 deft-directory org-directory
 org-agenda-files (directory-files-recursively "~/Dropbox/org-current" "org$")
 org-default-notes-file (expand-file-name "~/Dropbox/org-current/refile.org")
 org-tags-exclude-from-inheritance (quote ("project"))

 time-stamp-active t
 calendar-week-start-day 1)

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
     ;; at heading
     ((org-at-heading-p)
      ;; jump to the end of drawers&logbook & start typing text!
      ;; disallow jumping unless title is not empty
      (if (not (string= "" (org-entry-get nil "ITEM")))
        (let ((blanks-to-add (- 2 (org-element-property :post-blank (org-element-context)))))
          (org-end-of-meta-data t)
          (newline-and-indent blanks-to-add)
          (indent-according-to-mode))))

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
            (insert "\n\n"))
        ;; empty item
        (delete-region (line-beginning-position) (line-end-position))
        (delete-backward-char 1)
        (org-return-indent)
        ))
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

(map!
 :map org-agenda-mode-map
 "j" #'evil-next-line
 "k" #'evil-previous-line)

(map!
 :map org-super-agenda-header-map
 "j" #'evil-next-line
 "k" #'evil-previous-line)

(map! (:localleader
       :after evil-org
       :map evil-org-mode-map
       "q" #'utrack/org-toggle-tag))

(map! (:localleader
       :after org
       :map org-agenda-keymap
       "q" #'utrack/org-toggle-tag))

(defun utrack/org-ql-get-all-tags ()
  "Lookup and return a list of known tags."
  (delq nil (delete-dups (flatten-list
                          (org-ql-select (org-agenda-files)
                            '(tags)
                            :action #'(org-get-tags))))))


(defun utrack/org-toggle-tag ()
  "Interactively select a tag from org-files and toggle it for current
item, where item can be an exact heading in org buffer or org-agenda item.
Removes tag @unsorted if it wasn't selected manually."
  (interactive)
  (let* ((toggtag (completing-read
                   "Tag: " (utrack/org-ql-get-all-tags) nil nil ))
         (hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (point-marker)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (goto-char pos)
        (org-toggle-tag toggtag)
        (if (not (string= toggtag "@unsorted"))
            (org-toggle-tag "@unsorted" 'off))))
    ))

(require 'org-modern)
(setq
 org-startup-folded t
 org-hide-emphasis-markers t ;; hide *'s in *bold*, ~ in ~code~ etc
 org-imenu-depth 6
 ;; org-modern
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────"
 org-modern-variable-pitch nil


 )

(when window-system
  (let* (

         (headline           `(:inherit default :weight bold )))

    (custom-theme-set-faces 'user
                            `(org-level-8 ((t (,@headline ))))
                            `(org-level-7 ((t (,@headline ))))
                            `(org-level-6 ((t (,@headline ))))
                            `(org-level-5 ((t (,@headline ))))
                            `(org-level-4 ((t (,@headline ))))
                            `(org-level-3 ((t (,@headline ))))
                            `(org-level-2 ((t (,@headline :height 1.1 ))))
                            `(org-level-1 ((t (,@headline :height 1.1 :weight bold))))
                            `(org-document-title ((t (,@headline :height 1.25 :weight bold)))))))

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  (setq


   org-todo-keywords '((sequence "TODO(t)" "TODAY(n)" "|" "DONE(d)" "CNCL(c)")
                     (sequence "WAIT(w)" "|")
                     (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
                     (sequence "PROJ(p)" "|" "PFIN")
                     (sequence "TASK(s)" "|" "TFIN"))
   org-todo-keyword-faces '(;; next
                          ("TODO" . (:foreground "DarkSalmon"))
                          ("TODAY" . (:foreground "goldenrod1" :weight bold))
                          ("DONE" . (:foreground "ForestGreen"))
                          ("CNCL" . (:foreground "gray"))

                          ("WAIT" . (:foreground "PowderBlue" :weight bold))

                          ("PROJ" . (:foreground "DimGray"))
                          ("PFIN" . (:foreground "ForestGreen"))
                          ("TASK" . (:foreground "SlateGray"))
                          )

   org-use-fast-todo-selection t ;; hotkey C-c C-t
   org-fast-tag-selection-single-key t

   org-hierarchical-todo-statistics t

   org-log-done 'time
   ;; log TODO state changes
   org-log-into-drawer t

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

(defun my/org-capture-maybe-create-id ()
  (when (org-capture-get :create-id)
    (org-id-get-create)))
(add-hook 'org-capture-mode-hook #'my/org-capture-maybe-create-id)

(setq org-capture-templates '(
                              ("i" "Inbox" entry (file+headline org-default-notes-file "Inbox")
                               "* TODO [#B] %?\t:@unsorted:\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\nEntered on: %U\n")
                              ("p" "Inbox: Personal" entry (file+headline org-default-notes-file "Personal")
                               "* TODO [#B] %?\t :@personal:\nEntered on: %U\n")

                              ("d" "cl: dump immediately" plain (clock) "%i" :immediate-finish t :empty-lines 1)

                              ("s" "cl: subtask" entry (clock)
                               "* TODO %?\nref: %a\n%i" :empty-lines 1)
                              ("S" "cl: subtask snippet" entry (clock)
                               "* %?\n%(ha/org-capture-code-snippet \"%F\")" :empty-lines 1)
                              ("e" "cl: snip" plain (clock)
                               "%?\n%(ha/org-capture-code-snippet \"%F\")" :empty-lines 1)
                              ("i" "cl: new item" entry (clock)
                               "%?\nref: %a\n%i" :empty-lines 1)
                              ("m" "meeting template" entry
                               (file+headline "~/Dropbox/org-current/roam/meetingnotes.org" "Meetings")
                               (file "~/org/.roam-tpl/meetingnote.org")
                               :empty-lines 1 :create-id t :clock-in t :jump-to-captured t :immediate-finish t)
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

(defun utrack/hooks/org-mode-proj-cookie ()
  "Add counter cookie to items marked PROJ."
  (interactive)
  (if (equal (org-get-todo-state) "PROJ")
      (progn
        (org-set-property "COOKIE_DATA" "todo recursive")
        (org-back-to-heading t)
        (let* ((title (nth 4 (org-heading-components))))
          (if (not (string-prefix-p "[" title))
                         (progn(forward-whitespace 2)
             (insert "[/] ")) ))

        (org-update-statistics-cookies nil))))

(add-hook 'org-after-todo-state-change-hook
          'utrack/hooks/org-mode-proj-cookie)

(require 'org-ql)



(require 'org-ql-secretary)


(map!
 :map org-mode-map
 :ni "C-c p" #'org-ql-sec-insert-person-link)

(map!
 :map org-mode-map
 :localleader
 "E"  #'org-export-dispatch)

(map!
 :map org-mode-map
 :localleader
 :nv "e" nil)

(map!
 :map org-mode-map
 :localleader
:prefix "e"
:desc "Task view" "t"  #'org-ql-sec-show-task-view
 :desc "Assign" "a"  #'org-ql-sec-point-assign-to
 :desc "Mark as project" "p"  #'+utrack/org-mark-as-project
 :desc "Set with" "w"  #'org-ql-sec-set-with
 :desc "Meeting notes" "m" #'org-ql-sec-start-meeting)

(map!
 :prefix "C-c"
 (
  "s" #'org-ql-sec-show-task-view
  "w" #'org-ql-sec-start-meeting
  ))

(defun +utrack/org-mark-as-project ()
  "Mark object at point as project.
Headline gets PROJ todo state, file gets :project: tag added."
  (interactive)
  (if (org-get-heading) (org-todo "PROJ")
    (let ((tags (split-string (org-entry-get nil "FILETAGS") ":")))
      (let ((newtags (remove "" (delete-dups (append tags '("project"))))))
        (org-entry-put nil "FILETAGS" (concat ":" (string-join newtags ":") ":"))
        ))))

(after! org-ql
  (map! :leader
        :prefix "n"
        :desc "Agendas" "a" #'org-ql-view)
  (defun +utrack/org-ql-show-unsched ()
    "Show 'Unscheduled' org-ql view."
    (interactive)
    (org-ql-view "Unscheduled TODOs"))
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
(defun +utrack/org-ql-show-retro ()
    "Show 'Retro' view for a specific date."
    (interactive)
    (let ((seldate (org-read-date)))

      (org-ql-search
        (org-agenda-files)
        `(and (ts :on ,seldate))
        :title "retro"
        :narrow nil
        :buffer "*Org QL View: Retro*"
        :super-groups
        `(
          (:name "Done"
           :todo ("DONE" "PFIN" "TFIN"))
          (:name "Meetings"
           :tag "meeting")
          (:name "Todos"
           :todo t)
          (:auto-parent t)

          ))))
  (map! :leader
        :prefix "oa"
        :desc "Now" "n" #'+utrack/org-ql-show-now
        :desc "Pick" "p" #'+utrack/org-ql-show-pick
        :desc "Stuck" "s" #'+utrack/org-ql-show-stuck
        :desc "Retro" "r" #'+utrack/org-ql-show-retro
        :desc "Dangling" "d" #'+utrack/org-ql-show-unsched
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
                                 (todo "EPIC" "PROJECT")
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
                        (and
                         ;; filter Jira tix not assigned to me
                         (not (property "TYPE" "ejira-epic"))
                         (or
                          (not (property "TYPE" "ejira-issue"))
                          (tags "ejira_assigned"))

                         (or
                          (closed :on today) ;; log

                          (and
                           (or (todo) (done)) ;; touched this today
                           (or (ts-inactive :on today)
                               (descendants (ts-inactive :on today))))

                          (and
                           (not (done))
                           ;; also show everything real-TODAY or touched today
                           (and (scheduled :on today)
                                (todo "TODAY")))
                          ))
                        :sort todo
                        :super-groups (
                                       (:name "Done so far"
                                        :todo ("DONE" "EFIN" "CNCL" "THROWN"))
                                       (:habit t)
                                       (:name "Touched today"
                                        :not (:todo ("TODAY")))
                                       (:auto-parent t)
                                       )
                        )

                       )))

(set-popup-rules! '(
                    ("^\\*Org QL View" :side right :width +popup-shrink-to-fit :quit 'current :select t :modeline nil :vslot -1)
                    ("^\\*Org QL View: Now" :side right :width 0.4 :quit 'current :select t :modeline nil :vslot 2)
                    ))

(defun +utrack/org-paste-image ()
  "Paste an image into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))

  (shell-command (concat "xclip -selection clipboard -t image/png -o > " filename))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]")))
  )

(map! :localleader
      :after org
      :map evil-org-mode-map
      :prefix "a"
      "i" #'+utrack/org-paste-image
      )

(setq org-export-headline-levels 6)

) ;; end after! org
