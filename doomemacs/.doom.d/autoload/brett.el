;;; ~/.doom.d/autoload/brett.el -*- lexical-binding: t; -*-

;; https://raw.githubusercontent.com/Brettm12345/doom-emacs-literate-config/master/autoload/brett.el

;;;###autoload
(defun +brett/find-notes-for-major-mode (&optional arg)
  "Find org mode documentation for current major mode"
  (interactive)
  (let ((default-directory (expand-file-name "code/" +org-dir)))
    (if arg
        (call-interactively #'find-file)
      (find-file
       (expand-file-name (concat (string-remove-suffix "-mode" (symbol-name major-mode)) ".org"))))))

;;;###autoload
(defun +brett/find-notes-for-project (&optional arg)
  "Find notes for the current project"
  (interactive)
  (let ((default-directory (expand-file-name "projects/" +org-dir)))
    (if arg
        (call-interactively #'find-file)
      (find-file
       (expand-file-name (concat (doom-project-name 'nocache) ".org"))))))

(provide 'brett)
;;; brett.el ends here
