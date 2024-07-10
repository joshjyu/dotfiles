;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ORG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure t
  :config
  (setq org-directory "~/Documents/synced/org/"
    org-agenda-files (list org-directory)
    org-todo-keywords '((sequence
                          "TODO" "LT-TODO" "IDEA" "MAYBE"
                          "IN-PROGRESS" "WAITING" "CANCELED" "DONE"))
    org-hide-emphasis-markers t
    org-startup-indented t
    org-log-done 'time
    org-element-use-cache nil
    org-auto-align-tags nil
    org-tags-column 0
    org-special-ctrl-a/e t
    org-catch-invisible-edits 'show-and-error
    org-insert-heading-respect-content t
    org-pretty-entities t
    org-agenda-tags-column 0
    org-agenda-block-separator ?─
    org-agenda-time-grid
    '((daily today require-timed)
       (800 1000 1200 1400 1600 1800 2000)
       " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
    org-agenda-current-time-string
    "◀── now ─────────────────────────────────────────────────"
    ))

(setq org-ellipsis "…")
(set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

;; Custom capture templates
(setq org-capture-templates
  `(("i" "IDEA" entry (file+headline "inbox.org" "IDEAs")
      ,(concat "* IDEA %?\n"
         "/Entered on/ %U")
      :prepend t)
     ("t" "TODO" entry (file+headline "inbox.org" "TODOs")
       ,(concat "* TODO %?\n"
	  "/Entered on/ %U")
       :prepend t)
     ("l" "LT-TODO" entry (file+headline "inbox.org" "LT-TODOs")
       ,(concat "* LT-TODO %?\n"
	  "/Entered on/ %U")
       :prepend t)
     ))

;; Below line in case I want to add custom tags
;; (setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("@laptop" . ?l))

;; Org-journal mode
(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir org-directory
    org-journal-file-type 'monthly
    org-journal-file-format "%Y%m.org"
    org-journal-time-format ""
    org-journal-hide-entries-p nil
    org-journal-carryover-items "TODO=\"TODO\"|TODO=\"LT-TODO\"|TODO=\"IDEA\"|TODO=\"MAYBE\"|TODO=\"IN-PROGRESS\"|TODO=\"WAITING\""))

(provide 'init-org)
