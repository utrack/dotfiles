;;; ~/.dotfiles/doomemacs/.doom.d/+spotify.el -*- lexical-binding: t; -*-
;; customizations for [[https://github.com/danielfm/spotify.el][Spotify]]

;; TODO do we need it
(add-to-list 'load-path "~/.doom.d/packages/spotify.el")
(require 'spotify)
;; secrets provided in +local

;;; ~ Local keymaps
;; enable M-RET to select track/playlist
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

;;; ~ Global keymaps
;; use leader-2 to control playback
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
