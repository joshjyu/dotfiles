;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  ;; Defer LSP server startup until the buffer is visible
  :bind (("C-c C-d" . lsp-describe-thing-at-point))
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
  (lsp-apply-edits-after-file-operations nil))
;; Disable applying edits returned by server after file operations

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :bind (("C-c h" . lsp-ui-doc-focus-frame)
          ("C-c q" . lsp-ui-doc-hide))
  ;; To show doc at point: C-c p h g
  :config
  (setq
    ;; doc config
    lsp-ui-doc-enable t
    lsp-ui-doc-show-with-mouse nil
    lsp-ui-doc-use-childframe nil
    lsp-ui-doc-use-webkit nil
    ;; sideline config
    lsp-ui-sideline-show-diagnostics nil
    lsp-ui-sideline-show-code-actions nil
    ;; imenu config
    lsp-ui-imenu-auto-refresh t
    lsp-ui-imenu-buffer-position 'left
    lsp-ui-imenu-window-width 35
    ;; peek config
    lsp-ui-peek-enable t))

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
