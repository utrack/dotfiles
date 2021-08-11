;;; ../.dotfiles/doomemacs/.doom.d/my/org-ql-secretary.el -*- lexical-binding: t; -*-
(require 'org-ql)

;; TODOs:
;; 1) org-ql-sec-insert-link is too magical - it would be better to hook into
;;      org-roam-insert-link while setting the page name programmatically

(defun org-ql-sec--list-persons ()
  "Lookup and return a list of known persons."
  (delq nil (delete-dups (flatten-list
                          (org-ql-select (org-agenda-files)
                            '(or (property "PERSON") (property "MENTIONS"))
                            :action #'(append (org-entry-get-multivalued-property
                                               nil "PERSON")
                                              (org-entry-get-multivalued-property
                                               nil "MENTIONS")))))))

(defvar org-ql-sec-with nil
  "Value of the :PERSON: property when doing an
   org-ql-sec-tag-entry. Change it with org-ql-sec-set-with,
   set to C-c w.")

(defcustom org-ql-sec-persons-directory "~/Dropbox/org-current/people"
  "Path to org-mode files referencing persons.")

(defun org-ql-sec-set-with ()
  "Changes the value of the org-ql-sec-with variable for use in the
   next call of org-sec-tag-entry."
  (interactive)
  (setq org-ql-sec-with (completing-read "With: " (org-ql-sec--list-persons)
                                         nil 'confirm org-ql-sec-with)))

;; setup PERSON/MENTIONS search predicate for org-ql
(org-ql-defpred (person p) (&rest names)
  "Search for entries about any of NAMES."
  :normalizers ((`(,predicate-names . ,names)
                 `(or ,@(cl-loop for name in names
                                 collect `(org-entry-member-in-multivalued-property nil "mentions" ,name))
                      ,@(cl-loop for name in names
                                 collect `(property "person" ,name))))))

(defun org-ql-sec-show-task-view ()
  "Ask for a PERSON and show tasks associated with them."
  (interactive)
  (org-ql-sec-set-with)
  (org-ql-search (org-agenda-files)
    `(and (not (done)) (person ,org-ql-sec-with))
    :title (concat "Related to " org-ql-sec-with)
    :super-groups '((:name "My tasks"
                     :todo ("TODO" "TODAY"))
                    (:name "Delegated"
                     :todo ("TASK")
                     :property ("PERSON" org-ql-sec-with))
                    (:name "Mentions"
                     :auto-category t)
                    )
    ))

(defun org-ql-sec-insert-person-link ()
  "Insert a link to Roam page of ppl/(PERSON) and add this person to
MENTIONS of a current heading."
  (interactive)
  (org-roam-node-insert (lambda (node) (string-prefix-p
                                   "ppl/" (org-roam-node-title node) )))
  (let ((person-name (string-remove-prefix "ppl/"
                         (org-roam-node-title (org-roam-node-from-id
                                               (org-element-property :path (org-element-context))))
                         )))
  (org-back-to-heading t)
  (org-entry-add-to-multivalued-property
   nil "MENTIONS" person-name
   )))

(provide 'org-ql-secretary)
;;; org-ql-secretary.el ends here
