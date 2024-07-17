;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SIDEBAR, DIRED, IBUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add icons in dired mode
(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)
          (dired-mode . dired-hide-details-mode)))

;; Do not create new buffers for every directory
(setf dired-kill-when-opening-new-dired-buffer t)
;; Copy and delete recursively into directories
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq
    dired-sidebar-theme 'icons
    dired-sidebar-use-custom-font t
    dired-sidebar-no-delete-other-windows t
    dired-sidebar-one-instance-p t))
(add-hook 'dired-sidebar-mode-hook (lambda () (visual-line-mode -1)))

(use-package ibuffer-sidebar
  :ensure t
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq
    ibuffer-sidebar-use-custom-font t))

;; Default sorting order in ibuffer. Cycle with , key
(setq ibuffer-default-sorting-mode 'filename/process)

;; Global keybinding set to C-x C-n
(defun +sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (ibuffer-sidebar-toggle-sidebar)
  (dired-sidebar-toggle-sidebar))

(provide 'init-sidebar)
