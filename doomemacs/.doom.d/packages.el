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
(package! counsel-jq)
(package! ejira :recipe
  (:host github
   :repo "nyyManni/ejira"
   :files ("*.el")
   )
  )

(package! ox-jira) ;; org-mode export to jira format
(package! prism)
;;(package! org-jira)

(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
