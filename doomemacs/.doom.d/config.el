(require 'esup)
(add-to-list 'load-path "~/.doom.d/autoload")
(add-to-list 'load-path "~/.doom.d/my") ;; required by assistant / secretary

(setq undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      inhibit-compacting-font-caches t
      truncate-string-ellipsis "…")

(global-subword-mode +1) ; iterate through CamelCase
;;(global-visual-line-mode +1)
;;(+popup-mode +1)

(setq confirm-kill-emacs nil)

(setq
 counsel-outline-display-style (quote path)

 split-height-threshold 4
 split-width-threshold 160
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
;;(load! "+org-gcal")
;;(load! "+org-ejira")
(load! "+org-journal")
(load! "+sharing")
(load! "+shell")
(load! "+treemacs")
(load! "+ui")
(load! "+workspaces")

(require 'org-ql-secretary)

;;(load! "+excorporate")

;; (require 'doom-nano)
;; (nano-theme-dark)

;; (require 'nano-defaults)
;; (require 'nano-modeline)
;; (require 'nano-bindings)
;; (require 'nano-colors)
;; (require 'nano-help)
;; (require 'hide-header-line)
