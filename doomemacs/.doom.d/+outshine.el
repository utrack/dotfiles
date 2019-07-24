;;; ~/.dotfiles/doomemacs/.doom.d/+outshine.el -*- lexical-binding: t; -*-

;;; ~ Init
(add-hook 'outline-minor-mode-hook 'outshine-mode)

;; Enables outline-minor-mode for *ALL* programming buffers
(add-hook 'prog-mode-hook 'outline-minor-mode)

;; Narrowing now works within the headline rather than requiring to be on it
(advice-add 'outshine-narrow-to-subtree :before
            (lambda (&rest args) (unless (outline-on-heading-p t)
                                   (outline-previous-visible-heading 1))))

;;; ~ Keybindings
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
