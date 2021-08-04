;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! ivy-rich)
;;(package! dap-mode)
(package! zoom)
(package! evil-snipe)
(package! evil-visual-mark-mode)
(package! poporg)
(package! edit-indirect)
(package! git-link)
;;(package! protobuf-mode)
(package! org-super-agenda)
;;(package! org-gcal)
(package! org-ql)
(package! org-sidebar)
;;(package! burly)
(package! beacon)
(package! string-inflection)

(package! ejira :recipe
  (:host github
   :repo "nyyManni/ejira"
   :files ("*.el")
   )
  )
