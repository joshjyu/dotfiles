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

;; Company ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Company - text completion framework package
(use-package company
  :ensure t
  :config
  (setq company-idle-delay nil
    ;; Manually activate with <tab> (see global bindings)
    company-minimum-prefix-length 1
    company-transformers '(company-sort-prefer-same-case-prefix)))
;; Results are more sensible when preferring same case prefix

(with-eval-after-load 'company
  ;; Unbind RET from selecting completions, instead use TAB for that
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))
(add-hook 'prog-mode-hook #'company-mode)

;; Company-Box - Company frontend w/ icons
(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-frame-top-margin 8
    ;; Use C-h at point for doc
    company-box-doc-enable nil))

(provide 'init-minibuffer)
