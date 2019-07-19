(add-to-list 'load-path "~/.doom.d/autoload")
(setq user-full-name "Nikita Koptelov")
(setq user-mail-address "nick@koptelov.me")
(add-to-list 'load-path "~/.doom.d/packages/spotify.el")
(require 'spotify)
(require 'utrack-spotify-secrets)
(map!
 :after spotify
 (:mode spotify-track-search-mode
  :n "<M-return>" #'spotify-track-select
  )
 (:mode spotify-playlist-search-mode
   :n "<M-return>" #'spotify-track-select
   )
 (:localleader
   :map spotify-track-search-mode-map
   " " #'spotify-track-select)
 (:localleader
   :map spotify-playlist-search-mode-map
   " " #'spotify-track-select)
 )
(map!
:leader
      (:prefix-map ("2" . "spotify")
        :desc "Prev track"                 "h" #'spotify-previous-track
        :desc "Toggle"                 "j" #'spotify-toggle-play
        :desc "Next track"                 "l" #'spotify-next-track
        :desc "My playlists"                 "m" #'spotify-my-playlists
        )
      :after spotify
 )
(purpose-x-magit-single-on)
(purpose-x-golden-ratio-setup)
(purpose-compile-user-configuration)
(advice-add 'evil-delete-marks :after
              (lambda (&rest args)
                (evil-visual-mark-render)))
;; Required for outshine
(add-hook 'outline-minor-mode-hook 'outshine-mode)

;; Enables outline-minor-mode for *ALL* programming buffers
(add-hook 'prog-mode-hook 'outline-minor-mode)

;; Narrowing now works within the headline rather than requiring to be on it
(advice-add 'outshine-narrow-to-subtree :before
            (lambda (&rest args) (unless (outline-on-heading-p t)
                                   (outline-previous-visible-heading 1))))
(map!
 :leader
 :after outline
 :mode outline-minor-mode
 (:prefix-map ("l" . "outline")
   :desc "Subtree down"                 "j" #'outline-move-subtree-down
   :desc "Subtree up"                 "k" #'outline-move-subtree-up
   :desc "Promote"                 "h" #'outline-promote
   :desc "Demote"                 "l" #'outline-demote
   :desc "Narrow"  "n" #'outshine-narrow-to-subtree
   :desc "Widen"  "w" #'widen
   )
 )

(map!
 (:after outshine
   ;;:map outline-minor-mode-map
   :n "<tab>" #'outshine-cycle
   :n "<backtab>" #'outshine-cycle-buffer
   :n "<M-return>" #'outshine-insert-heading
   :n "gh" #'outline-up-heading
   :n "gj" #'outline-forward-same-level
   :n "gk" #'outline-backward-same-level
   :n "[h" #'outline-previous-visible-heading
   :n "]h" #'outline-next-visible-heading

   :n "M-h" #'outline-promote
   :n "M-l" #'outline-demote
   :i "M-h" #'outline-promote
   :i "M-l" #'outline-demote
   )
 )

(map! :leader
      (:prefix "/"
        :desc "Search project" "/" #'+default/search-project)
      )
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
      )
(map! :leader
        :desc "Capture note"       "4"  #'org-capture
      )
(map! :leader
        :desc "Imenu"       "3"  #'imenu
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
;; Fix annoying vertical window splitting.
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
(with-eval-after-load "window"
  (defcustom split-window-below nil
    "If non-nil, vertical splits produce new windows below."
    :group 'windows
    :type 'boolean)

  (defcustom split-window-right nil
    "If non-nil, horizontal splits produce new windows to the right."
    :group 'windows
    :type 'boolean)

  (fmakunbound #'split-window-sensibly)

  (defun split-window-sensibly
      (&optional window)
    (setq window (or window (selected-window)))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (split-window window nil (if split-window-right 'left  'right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (split-window window nil (if split-window-below 'above 'below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding the
             ;; value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil (if split-window-right
                                              'left
                                            'right))))))))

(setq-default split-height-threshold  4
              split-width-threshold   160) ; the reasonable limit for horizontal splits
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
(defun utrack/is-url (string)
  (let ((url  "\\(http[s]?://\\|www\\.\\)"))
    (string-match url link)
    )
)

  (defun utrack/clipboard-as-org-link (title)
    "If there's a URL on the clipboard, return it as an org-mode
link in the form of [[url][title]], else concat url title"
    (let ((link (substring-no-properties (x-get-selection 'CLIPBOARD))))
        (if (utrack/is-url link)
              (concat "[[" link "][" title "]]")
              (concat link " " title)
          )))
(after! org
  :config
  (setq +org-dir org-directory
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-capture-templates
        '(("c" "Code Task" entry (file+headline org-default-notes-file "Coding Tasks")
           "* TODO %?\n  Entered on: %U - %a\n")

          ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO [#B] %?\n  Entered on: %U\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))")

          ("x" "Context Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO [#B] %?\n  Entered on: %U\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a")

          ("r" "Reading List" entry (file+headline org-default-notes-file "Reading")
           "* [ ] %(utrack/clipboard-as-org-link \"%?\")\n  Entered on: %U\n")

          ("n" "Note" entry (file+olp+datetree org-default-notes-file)
           "* %?\n\n"))))
(setq display-line-numbers-type 'relative)
(defun theme-picker ()
  (interactive)
  (ivy-read "Select a theme"
            '(
              afternoon
              hc-zenburn
              )
            :require-match t
            :action (lambda (x)
                      (load-theme x t))))
(after! doom-themes
  (setq
   doom-themes-enable-bold t
   doom-themes-enable-italic t))
(after! doom-modeline
  (setq doom-modeline-bar-width 3))
(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'relative-from-project))
(add-hook 'treemacs-mode #'treemacs-follow-mode)
(after! magit
  (setq magit-display-buffer-function #'magit-display-buffer-traditional))
(add-to-list 'display-buffer-alist
             `(,(rx bos "*magit:")
               (display-buffer-reuse-window
                display-buffer-below-selected)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))
