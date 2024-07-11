(setq-default mode-line-percent-position nil)
(line-number-mode)
(column-number-mode)

;; Change "git-" in vc indicator. Using all-the-icons git glyph.
;; C-h v all-the-icons-data/* for data alist
(setcdr (assq 'vc-mode mode-line-format)
  '((:eval (replace-regexp-in-string "^ Git\-" " " vc-mode))))

;; Remove process ID in LSP name in mode line
;; github.com/emacs-lsp/lsp-mode/discussions/3729#discussioncomment-3689046
(defun my-custom-lsp--workspace-print (workspace)
  "Visual representation WORKSPACE."
  (let* ((proc (lsp--workspace-cmd-proc workspace))
          (status (lsp--workspace-status workspace))
          (server-id
            (-> workspace
              lsp--workspace-client lsp--client-server-id symbol-name)))
    (if (eq 'initialized status)
      (format "%s" server-id)
      (format "%s/%s" server-id status))))

;; Override default lsp--workspace-print with custom one
(advice-add #'lsp--workspace-print :override #'my-custom-lsp--workspace-print)

;; Delight customizes modes in the mode line
(use-package delight
  :ensure t)
;; Delight can also be used in use-package configs but I find it cleaner
;; to do it separately like this
;; Syntax note for minor modes: (delight SPEC VALUE FILE)
;; SPEC = mode symbol, VALUE = replacement name, FILE = library that defines
;; the minor mode (can be different like in visual-line-mode)
(delight '((hs-minor-mode nil hideshow)
            (yas-minor-mode nil yasnippet)
            (company-mode nil company)
            (company-box-mode nil company-box)
            (which-key-mode nil which-key)
            (jinx-mode nil jinx)
            (eldoc-mode nil eldoc)
            (org-indent-mode nil org-indent)
            (adaptive-wrap-prefix-mode nil adaptive-wrap)
            (visual-line-mode nil simple)
            (auto-revert-mode nil autorevert)
            (lsp-lens-mode nil lsp-lens)))

(provide 'init-mode-line)
