;;; hide-header-line.el --- hook that hides header-line when hide-mode-line is set -*- lexical-binding: t; -*-
;;
;; Author: Nikita Koptelov <http://github/utrack>
;; Maintainer: Nikita Koptelov <nick@koptelov.me>
;; Created: December 11, 2020
;; Version: 1.0.1
;; Keywords: frames hide-mode-line header-line
;; URL: https://github.com/utrack/emacs-hide-header-line
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Adds a hook that hides `header-line` as well when `hide-mode-line' is active.
;;
;;; Code:
(require 'hide-mode-line)

(defvar-local hide-header-line--old-format nil
  "Storage for the old `header-line-format', so it can be restored when
`hide-mode-line-mode' is disabled.")

(defvar hide-header-line-format nil
  "The modeline format to use when `hide-mode-line-mode' is active.")

(defun hide-mode-line-header ()
  "Toggle header line in sync with hide-mode-line-mode."
  (if hide-mode-line-mode
      (setq hide-header-line--old-format header-line-format
            header-line-format hide-header-line-format)
    (setq header-line-format hide-header-line--old-format))
  (force-mode-line-update))

;;;autoload
(add-hook `hide-mode-line-mode-hook 'hide-mode-line-header)
(provide 'hide-header-line)
;;; hide-header-line.el ends here
