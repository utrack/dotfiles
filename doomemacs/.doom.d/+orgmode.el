(after! org

  (add-hook 'org-mode-hook 'auto-fill-mode)
  (setq org-src-fontify-natively t
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

        org-todo-keywords '((sequence "TODO(t)" "TODAY(n)" "|" "DONE(d)" "CNCL(c)")
                            (sequence "PROJECT(p)" "|" "DONE")
                            (sequence "WAITING(w)" "EXPAND(e)" "|")
                            (sequence "DELEGATED(g)" "|" "THROWN(x)"))
        org-todo-keyword-faces '(;; next
                                 ("TODAY" . (:foreground "OrangeRed" :weight bold))
                                 ("TODO" . (:foreground "OrangeRed"))
                                 ("DONE" . (:foreground "LimeGreen"))
                                 ("CNCL" . (:foreground "gray"))
                                 ("WAITING" . (:foreground "PowderBlue" :weight bold))
                                 ("DELEGATED" . (:foreground "SlateGray"))
                                 ;; thrown
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
 org-imenu-depth 6)

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

(defun +utrack/org-capture-to-clock-subtasks ()
  "Sets org-capture point to 'Subtasks' item under clocked item.
Creates new subitem if not exists."
  (let (
        (m (cond
            ((org-clocking-p) org-clock-marker)

            ((and org-clock-goto-may-find-recent-task
                  (car org-clock-history)
                  (marker-buffer (car org-clock-history)))
             (car org-clock-history))
            (t (error "No active or recent clock task")))))
    ;; (with-current-buffer
    ;;     (marker-buffer m)
    (progn
      (switch-to-buffer (marker-buffer m))
      (goto-char m)
      (let ((child-level (+ 1 (org-current-level)))
            (candidates))
        ;; Search for immediate child "Subtasks"
        (org-map-entries (lambda ()
                           (if (and (eq child-level (org-current-level))
                                    (string= (org-entry-get (point) "ITEM") "Subtasks"))
                               (push (point) candidates))
                           ) nil 'tree)
        (cond
         ;; use existing Subtasks if exists
         ((car candidates) (goto-char (car candidates)))
         (t (org-insert-heading-respect-content)
            (org-do-demote)
            (insert "Subtasks")))
        )
      (org-capture-put-target-region-and-position)
      (widen)
      )))

(setq org-capture-templates '(
                              ("i" "Inbox" entry (file+headline org-default-notes-file "Inbox")
                               "* TODO [#B] %?\t:@unsorted:\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\nEntered on: %U\n")
                              ("p" "Inbox: Personal" entry (file+headline org-default-notes-file "Personal")
                               "* TODO [#B] %?\t :@personal:\nEntered on: %U\n")

                              ("c" "cl: capture an item" item (clock) "%i\n  %?" :empty-lines 1)
                              ("h" "cl: dump immediately" plain (clock) "%i" :immediate-finish t :empty-lines 1)

                              ("d" "cl: snip and describe entry" entry (clock)
                               "* %?\n%(ha/org-capture-code-snippet \"%F\")" :empty-lines 1)
                              ("e" "cl: snip and describe" plain (clock)
                               "%?\n%(ha/org-capture-code-snippet \"%F\")" :empty-lines 1)
                              ("i" "cl: new item" entry (clock)
                               "%?\nref: %a\n%i" :empty-lines 1)
                              ("s" "cl: subtask" entry (function +utrack/org-capture-to-clock-subtasks)
                               "* TODO %?\nEntered on: %U\n\nref: %a")
                              ("S" "cl: subtask with snip" entry (function +utrack/org-capture-to-clock-subtasks)
                               "* TODO %?\nEntered on: %U\n\n%(ha/org-capture-code-snippet \"%F\")")
                              ))

(defun utrack/notes-path-for-project ()
  ;; Open roam file "Project 'name'.org"
  (interactive)
  (let ((project-root (doom-project-name))
        (default-directory (expand-file-name "roam/" org-directory)))
    (expand-file-name (concat "Project " project-root ".org")))
  )

(setq

 ;; open agenda window in a new frame to the right
 org-agenda-window-setup (quote reorganize-frame)

 org-agenda-dim-blocked-tasks t
 ;; don't scan org files every time I open agenda buffer
 org-agenda-sticky t

 org-agenda-inhibit-startup nil
 org-agenda-start-day nil ;; today

 org-agenda-time-grid '((daily today require-timed) nil "----------------------")
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-deadline-if-done t
 org-agenda-include-deadlines t
 org-agenda-include-diary t
 org-agenda-block-separator nil
 org-agenda-compact-blocks t

 ;; org-agenda-sorting-strategy (quote
 ;;                              ((agenda todo-state-down deadline-up priority-down habit-down)
 ;;                               (todo priority-down category-keep)
 ;;                               (tags priority-down category-keep)
 ;;                               (search category-keep)))
 )

;; custom fonts for agenda

(setq org-fontify-done-headline t)

;; strikethrough done headlines
(custom-set-faces
 '(org-done ((t (
                 :weight light
                 :strike-through nil))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:strike-through t)))))

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

(use-package! org-super-agenda
  :commands (org-super-agenda-mode))
(after! org-agenda
  (org-super-agenda-mode)
  (setq org-agenda-custom-commands '())

  (add-to-list 'org-agenda-custom-commands
               '("m" "Morning Pick"
                 ((agenda "" ( (org-agenda-span 'day)
                               (org-super-agenda-groups
                                '(
                                  (:discard (:habit t))
                                  (:name "Today"
                                   :time-grid t
                                   :and (
                                         :todo "TODAY"
                                         :scheduled today)
                                   :order 1)

                                  (:name "Overdue TODAYs"
                                   :and (
                                         :todo "TODAY"
                                         :scheduled past)
                                   :and (
                                         :todo "TODAY"
                                         :deadline past)
                                   :order 10)
                                  (:name "Overdue"
                                   :scheduled past
                                   :deadline past
                                   :order 11)
                                  (:name "Candidates"
                                   :scheduled today
                                   :deadline today
                                   :order 12)
                                  (:discard (:anything t))
                                  )
                                )))
                  (alltodo "" ((org-agenda-overriding-header "")
                               (org-super-agenda-groups
                                '(
                                  (:discard (:habit t))
                                  (:discard (:tag ("DESIGNDOC")))
                                  (:discard (:and (:todo "TODAY" :scheduled today)))
                                  (:name "Important"
                                   :tag "Important"
                                   :priority "A"
                                   :order 6)
                                  (:name "Reading list"
                                   :tag "Read"
                                   :order 30)
                                  (:name "Waiting"
                                   :todo "WAITING"
                                   :order 20)
                                  (:auto-category t :order 99))))))))

  (add-to-list 'org-agenda-custom-commands
               '("n" "Today's agenda"
                 ((agenda "" (
                              (org-agenda-start-with-log-mode t)
                              (org-agenda-show-log '(closed state))
                              (org-agenda-span 'day)
                              (org-super-agenda-groups

                               '(
                                 (:name "Today so far"
                                  :log changed
                                  :log closed)
                                 (:discard (:log t))
                                 (:habit t)

                                 (:discard (:not
                                            (:and (:todo "TODAY" :scheduled today))))

                                 (:auto-parent t)
                                 )
                               )))
                  (alltodo "" ((org-agenda-overriding-header "")
                               (org-super-agenda-groups
                                '(
                                  (:name "Projects"
                                   :todo "PROJECT")
                                  (:discard (:anything t))
                                  )))))))
  )

) ;; end after! org
