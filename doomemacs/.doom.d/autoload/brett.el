;;; ~/.doom.d/autoload/brett.el -*- lexical-binding: t; -*-

;; https://raw.githubusercontent.com/Brettm12345/doom-emacs-literate-config/master/autoload/brett.el

;;;###autoload
(defun +brett/find-notes-for-major-mode (&optional arg)
  "Find org mode documentation for current major mode"
  (interactive)
  (let ((default-directory (expand-file-name "code/" org-directory)))
    (if arg
        (call-interactively #'find-file)
      (find-file
       (expand-file-name (concat (string-remove-suffix "-mode" (symbol-name major-mode)) ".org"))))))

(provide 'brett)
;;; brett.el ends here
