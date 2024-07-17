;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MINIBUFFER/COMPLETION PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico to enable vertical view in minibuffer
(use-package vertico
  :ensure t
  :config
  (setq vertico-resize nil)
  (vertico-mode))

;; Marginalia to enable annotations next to entries in the minibuffer
(use-package marginalia
  :ensure t)
(marginalia-mode)

;; Consult - note the global bindings in global keybindings section
(use-package consult
  :ensure t)

;; Orderless - orderless completion style
(use-package orderless
  :ensure t
  :demand t
  :after minibuffer 
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Which-key to display keybinding completion option
(use-package which-key
  :ensure t)
(which-key-mode)

;; Company - text completion framework package
(use-package company
  :ensure t
  :config
  (setq
    company-tooltip-offset-display 'lines
    ;; Manually activate with <tab> (see global bindings)
    company-idle-delay nil
    company-minimum-prefix-length 1
    ;; Results are more sensible when preferring same case prefix
    company-transformers '(company-sort-prefer-same-case-prefix)
    company-format-margin-function 'company-text-icons-margin
    company-text-icons-add-background t))

(with-eval-after-load 'company
  ;; Unbind RET from selecting completions, instead use TAB for that
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

(add-hook 'prog-mode-hook #'company-mode)

(provide 'init-minibuffer)
