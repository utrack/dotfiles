(add-to-list 'load-path "~/.doom.d/autoload")
(add-to-list 'load-path "~/.doom.d/nano")
(add-to-list 'load-path "~/.doom.d/my")

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
(after! zoom
  (setq zoom-size (quote (0.624 . 0.624)))
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
(load! "+org-ejira")
(load! "+org-journal")
(load! "+sharing")
(load! "+shell")
(load! "+treemacs")
(load! "+ui")
(load! "+workspaces")

(require 'nano-faces)
(nano-faces)
(require 'nano-theme)
(nano-theme)

(require 'nano-defaults)
;;(require 'nano-session)
(require 'nano-modeline)
(require 'nano-bindings)
(require 'nano-counsel)
(require 'nano-layout)
(require 'nano-colors)
(require 'nano-help)
(require 'hide-header-line)
