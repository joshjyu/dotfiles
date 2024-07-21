;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  ;; Defer LSP server startup until the buffer is visible
  :init
  (setq lsp-keymap-prefix "C-c p")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
          (lsp-mode . lsp-diagnostics-mode)
          (tsx-ts-mode . lsp-deferred)
          (typescript-ts-mode . lsp-deferred)
          (js-ts-mode . lsp-deferred)
          (js-mode . lsp-deferred)
          (mhtml-mode . lsp-deferred)
          (css-ts-mode . lsp-deferred))
  :custom
  (read-process-output-max (* 1024 1024))
  ;; Increase the amount of data which Emacs reads from the process
  ;; Emacs default is ~4k, some language server responses are 800k-3M
  (lsp-completion-provider :none)               ; Using Company
  (lsp-keep-workspace-alive nil)                ; Kill server when not using
  (lsp-log-io nil)                              ; Can turn on if troubleshooting
  (lsp-auto-configure t)
  ;; Auto-configure is t by default - auto-configures lsp-ui and company
  (lsp-enable-indentation nil)                  ; Use language indentation rules
  (lsp-apply-edits-after-file-operations nil)
  ;; Disable applying edits returned by server after file operations
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-treemacs
  :ensure t
  :after lsp
  :custom
  (setq treemacs-no-delete-other-windows nil))

;; Sets lsp-treemacs window position param to right
(setq treemacs-position 'right)

(use-package dap-mode
  :ensure t
  :custom
  (lsp-enable-dap-auto-configure t)
  :config
  ;; (require 'dap-firefox)
  ;; (dap-firefox-setup)
  ;; dap-firefox seems to have been broken for awhile
  ;; github.com/emacs-lsp/dap-mode/issues/547
  ;; Just use developer tools
  (require 'dap-node)
  (dap-node-setup)
  (dap-ui-mode 1))

(provide 'init-lsp)
