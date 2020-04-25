(after! org

  (add-hook 'org-mode-hook 'auto-fill-mode)
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-return-follows-link t

        org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3))

        +org-directory (expand-file-name "~/Dropbox/org-current")
        +org-dir org-directory
        deft-directory org-directory
        org-agenda-files (directory-files-recursively "~/Dropbox/org-current/roam" "org$")
        org-default-notes-file (expand-file-name "~/Dropbox/org-current/roam/refile.org")

        org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d)" "CNCL(c)")
                            (sequence "WAITING(w)" "EXPAND(e)" "|")
                            (sequence "DELEGATED(g)" "|" "THROWN(x)"))
        org-todo-keyword-faces '(;; next
                                 ("TODO" . (:foreground "OrangeRed" :weight bold))
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

(setq org-fontify-done-headline t)

;; strikethrough done headlines
(custom-set-faces
 '(org-done ((t (
                 :weight bold
                 :strike-through t))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:strike-through t)))))

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

(setq org-capture-templates '(
                              ("i" "Inbox" entry (file+headline org-default-notes-file "Inbox")
                               "* TODO [#B] sort: %?\t:@unsorted:\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\nEntered on: %U\n")
                              ("a" "Inbox, ref at point" entry (file+headline org-default-notes-file "Inbox")
                               "* TODO [#B] sort: %(doom-project-name): %?\t:@unsorted:@p-%(doom-project-name):\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\nEntered on: %U\nref: %a")

                              ("p" "Inbox: Personal" entry (file+headline org-default-notes-file "Personal")
                               "* TODO [#B] %?\t :@personal:\nEntered on: %U\n")

                              ("n" "Project Note" entry (file+headline utrack/notes-path-for-project "Capture")
                               "* %?\n  Entered on: %U\n%a")

                              ("c" "cl: capture an item" item (clock) "%i\n  %?" :empty-lines 1)
                              ("h" "cl: dump item immediately" plain (clock) "%i" :immediate-finish t :empty-lines 1)
                              ("d" "cl: snip and describe" plain (clock) "%?\n%(ha/org-capture-code-snippet \"%F\")" :empty-lines 1)
                              ("e" "cl: new entry" entry (clock)
                               "* %?\nref: %a\n%i" :empty-lines 1)

                              ("s" "cl: subtask" entry (clock)
                               "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\nEntered on: %U\n\nref: %a")
                              ("S" "cl: subtask with snip" entry (clock)
                               "* TODO %?\nEntered on: %U\n\n%(ha/org-capture-code-snippet \"%F\")")
                              ))

(defun utrack/notes-path-for-project ()
  (interactive)
  (let ((project-root (doom-project-name))
        (default-directory (expand-file-name "roam/" org-directory)))
    (expand-file-name (concat "Project " project-root ".org")))
  )


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

  (setq

   ;; open agenda window in a new frame to the right
   org-agenda-window-setup (quote reorganize-frame)

   org-agenda-dim-blocked-tasks t
   org-agenda-persistent-filter t
   ;; don't scan org files every time I open agenda buffer
   org-agenda-sticky t

   ;;org-adapt-indentation nil
   ;;
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
                                ((agenda todo-state-down deadline-up priority-down habit-down)
                                 (todo priority-down category-keep)
                                 (tags priority-down category-keep)
                                 (search category-keep)))
   )

  (defun my/org-match-at-point-p (match)
    "Return non-nil if headline at point matches MATCH.
Here MATCH is a match string of the same format used by
`org-tags-view'."
    (funcall (cdr (org-make-tags-matcher match))
             (org-get-todo-state)
             (org-get-tags-at)
             (org-reduced-level (org-current-level))))
  ;; https://stackoverflow.com/questions/10074016/org-mode-filter-on-tag-in-agenda-view/33444799#33444799
  (defun my/org-agenda-skip-without-match (match)
    "Skip current headline unless it matches MATCH.

Return nil if headline containing point matches MATCH (which
should be a match string of the same format used by
`org-tags-view').  If headline does not match, return the
position of the next headline in current buffer.

Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines that do not match."
    (save-excursion
      (unless (org-at-heading-p) (org-back-to-heading))
      (let ((next-headline (save-excursion
                             (or (outline-next-heading) (point-max)))))
        (if (my/org-match-at-point-p match) nil next-headline))))

  (setq org-agenda-custom-commands '(
                               ("n" "Agenda and TODOs"
                                ((tags "PRIORITY=\"A\""
                                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                        (org-agenda-overriding-header "Top priority")))
                                 (agenda "")
                                 (alltodo ""
                                          ((org-agenda-skip-function
                                            '(my/org-agenda-skip-without-match "-DESIGNDOC"))
                                           (org-agenda-overriding-header "TODOs ignoring design docs"))
                                          )
                                 ))
                               ))

) ;; end after! org
