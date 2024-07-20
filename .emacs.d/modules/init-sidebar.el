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
    dired-sidebar-use-one-instance t
    dired-sidebar-use-magit-integration t
    dired-sidebar-mode-line-format nil))

(use-package ibuffer-sidebar
  :ensure t
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq
    ibuffer-sidebar-use-custom-font t
    ibuffer-sidebar-mode-line-format nil))

;; The text wrapping looks strange in the sidebar modes so turn off
;; visual line mode in dired and ibuffer sidebar modes
(dolist
  (hook '(dired-sidebar-mode-hook ibuffer-sidebar-mode-hook))
  (add-hook hook (lambda () (visual-line-mode -1))))

;; Filter groups for ibuffer
(setq ibuffer-saved-filter-groups
  '(("MyList"
      ("Dired" (or (derived-mode . dired-mode)
                 (derived-mode . dired-sidebar-mode)))
      ("Stars" (starred-name))
      ("Proc" (process))
      ("Emacs" (filename . ".emacs.d*"))      
      ("Org" (or (file-extension . "org")
               (derived-mode . org-mode)
               (derived-mode . org-agenda-mode)))
      ("Prog" (or (derived-mode . prog-mode)
                (derived-mode . mhtml-mode)))
      )))

;; Tell ibuffer and ibuffer-sidebar to load the filter group automatically
(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "MyList")))

;; Reverse order of ibuffer filter groups but keep [Default] at the bottom
;; First put default at the top
;; Define function to move first element of a list to the last position
;; see: emacswiki.org/emacs/IbufferFilters#h5o-4
(defun car-to-last (lst)
  (append (cdr lst) (list (car lst))))

;; [Default] filter group is normally generated first
;; Have ibuffer call the above function, putting [Default] at end of the list
;; Which puts it at the top of the buffer list
(advice-add 'ibuffer-generate-filter-groups
  :filter-return #'car-to-last)

;; Now, reverse the order of ibuffer filter groups after [Default] is moved
;; First define function to reverse lists
(defun jjy/reverse-list (x)
  (setq x (nreverse x)))
;; Then apply reversal to ibuffer-generate-filter-groups
(advice-add 'ibuffer-generate-filter-groups
  :filter-return #'jjy/reverse-list)

;; Custom toggle for combining both dired and ibuffer sidebars
;; Global keybinding set to C-x C-n
(defun +sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (ibuffer-sidebar-toggle-sidebar)
  (dired-sidebar-toggle-sidebar))

(provide 'init-sidebar)
