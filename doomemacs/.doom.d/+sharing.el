;;; ~/.dotfiles/doomemacs/.doom.d/+sharing.el -*- lexical-binding: t; -*-

(defun +utrack/copy-file-name-relative ()
  "Copy the current buffer file path in project to the clipboard."
  (interactive)
  (let ((filename (file-relative-name buffer-file-name (projectile-project-root))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(map! :leader
      :desc "Copy filename"       "cC"  #'+utrack/copy-file-name-relative
      )
