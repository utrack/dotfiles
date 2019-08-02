(add-to-list 'load-path "~/.doom.d/autoload")

(setq confirm-kill-emacs nil)
(global-visual-line-mode +1)
(setq doom-font (font-spec :family "Fira Code" :size 17))

(load! "+completion")
(load! "+core")
(load! "+editing")
(load! "+evil-utrack")
(load! "+keybinds")
(load! "+lsp")
(load! "+lang-go")
(load! "+local")
(load! "+magit")
(load! "+org")
(load! "+outshine")
;; (load! "+purpose")
(load! "+shell")
(load! "+spotify")
(load! "+treemacs")
(load! "+ui")
(load! "+workspaces")
