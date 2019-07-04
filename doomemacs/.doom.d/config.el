(add-to-list 'load-path "~/.doom.d/autoload")
(setq user-full-name "Nikita Koptelov")
(setq user-mail-address "nick@koptelov.me")
(add-to-list 'load-path "~/.doom.d/packages/spotify.el")
(require 'spotify)
(require 'utrack-spotify-secrets)
(global-spotify-remote-mode 1)
(map! (:localleader
          :map spotify-track-search-mode-map
          " " #'spotify-track-select))
(map! (:localleader
          :map spotify-playlist-search-mode-map
          " " #'spotify-track-select))
(global-unset-key (kbd "M-RET"))
(map! :leader
      (:prefix "b"
        :desc "Delete buffer" "d" #'kill-current-buffer)
      (:prefix "w"
        :desc "Delete window" "d" #'+workspace/close-window-or-workspace)
)
(map! :leader
      (:prefix "n"
        :desc "Browse mode notes"    "m" #'+brett/find-notes-for-major-mode
        :desc "Browse project notes" "p" #'+brett/find-notes-for-project)
)
(map! :leader
      (:prefix "TAB"
        :desc "Rename workspace"       "r"  #'+workspace/rename)
      (:prefix "n"
        :desc "Browse mode notes"    "m" #'+brett/find-notes-for-major-mode
        :desc "Browse project notes" "p" #'+brett/find-notes-for-project)
)
(map!
 (:after evil
   :en "C-h"   #'evil-window-left
   :en "C-j"   #'evil-window-down
   :en "C-k"   #'evil-window-up
   :en "C-l"   #'evil-window-right))
(map! (:localleader
        (:after evil-org
          :map evil-org-mode-map
          "/" #'counsel-org-goto)))
(map!
 (:after treemacs-evil
   (:map evil-treemacs-state-map
     "C-h" #'evil-window-left
     "C-l" #'evil-window-right)))
(after! which-key
  (setq which-key-idle-delay 0.2
        which-key-idle-secondary-delay 0.01
        which-key-sort-order 'which-key-key-order-alpha))
(setq doom-scratch-buffer-major-mode t)
(setq frame-resize-pixelwise t)
(setq show-trailing-whitespace t)
(setq eldoc-idle-delay 0.01)
(global-eldoc-mode 1)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)
(add-hook 'org-mode-hook 'eldoc-mode)
(after! company-box
  (setq company-box-max-candidates 5))
(def-package! company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))
(after! company
  (setq company-tooltip-limit 5
        company-tooltip-minimum-width 80
        company-tooltip-minimum 5
        company-backends
        '(company-capf company-dabbrev company-files company-yasnippet)
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)))
(setq +ivy-project-search-engines '(rg))
(def-package! counsel-tramp
  :commands (counsel-tramp))
(after! parinfer
  (setq parinfer-auto-switch-indent-mode t))
(setq +workspaces-on-switch-project-behavior t)
(add-hook 'go-mode-hook #'lsp)
(map! (:localleader
          :map go-mode-map
          "g" #'lsp-find-definition))
(after! org-mode
  (setq +org-directory (expand-file-name "~/org")
        org-agenda-files (list org-directory)))
(setq org-ellipsis " ▼ ")
(after! org
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)))
(after! org
  (setq
   org-refile-targets '((nil :maxlevel . 5)
                        (org-agenda-files :maxlevel . 5))))
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (
                 :weight bold
                 :strike-through t))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:strike-through t)))))
(after! org
  :config
  (setq +org-dir org-directory
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-capture-templates
        '(("c" "Code Task" entry (file+headline org-default-notes-file "Coding Tasks")
           "* TODO %?\n  Entered on: %U - %a\n")
          ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  Entered on: %U")
          ("n" "Note" entry (file+olp+datetree org-default-notes-file)
           "* %?\n\n"))))
(setq display-line-numbers-type 'relative)
;; (add-to-list '+doom-solaire-themes '(soft-stone . t))
;; ;; (setq doom-theme 'soft-stone)
;; (after! doom-themes
;;   (setq
;;    doom-themes-enable-bold t
;;    doom-themes-enable-italic t))
(after! doom-modeline
  (setq doom-modeline-bar-width 3))
(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'relative-from-project))
(add-hook 'treemacs-mode #'treemacs-follow-mode)
