;;; ~/.dotfiles/doomemacs/.doom.d/+evil-utrack.el -*- lexical-binding: t; -*-

(after! evil-snipe
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  (setq
   evil-snipe-spillover-scope 'buffer
   evil-snipe-override-evil-repeat-keys t
   )
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  )

(after! evil
  (setq
   evil-goggles-enable-change t
   evil-goggles-enable-delete t
   evil-visual-mark-mode t
   )
  )
