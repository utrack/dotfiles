(add-to-list 'load-path "~/.doom.d/autoload")

(setq user-full-name "Nikita Koptelov"
      user-mail-address "nick@koptelov.me")

(setq undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      inhibit-compacting-font-caches t
      truncate-string-ellipsis "…")

(global-subword-mode +1) ; iterate through CamelCase
(global-visual-line-mode +1)
(+popup-mode +1)

(setq confirm-kill-emacs nil)

(setq doom-font (font-spec :family "Fira Code" :size 17))
(setq doom-theme 'doom-peacock)

(setq
 counsel-outline-display-style (quote path)

 split-height-threshold 4
 split-width-threshold 160
 zoom-size (quote (0.624 . 0.624))
)


(load! "+completion")
(load! "+core")
(load! "+editing")
(load! "+evil-utrack")
(load! "+ivy")
(load! "+keybinds")
(load! "+lsp")
(load! "+lang-go")
(load! "+local")
(load! "+magit")
(load! "+orgmode")
(load! "+org-export")
(load! "+org-journal")
;; (load! "+outshine")
;; (load! "+purpose")
(load! "+sharing")
(load! "+shell")
;;(load! "+spotify")
(load! "+treemacs")
(load! "+ui")
(load! "+workspaces")
