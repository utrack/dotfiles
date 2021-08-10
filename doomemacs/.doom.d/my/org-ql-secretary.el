;;; ../.dotfiles/doomemacs/.doom.d/my/org-ql-secretary.el -*- lexical-binding: t; -*-
(require 'org-ql)


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
                                 collect `(property "mentions" ,name))
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
(global-set-key "\C-cs" 'org-ql-sec-show-task-view)


(provide 'org-ql-secretary)
;;; org-ql-secretary.el ends here
