;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BASIC CHANGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add modules path
(add-to-list 'load-path (concat user-emacs-directory
                          (convert-standard-filename "modules/")))

;; Separate custom.el file for custom-set-* clutter
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Save ~ files in a backup directory (instead of buffer's dir)
(setq backup-directory-alist '(("." . "~/.ebackup/"))
  backup-by-copying t          ; Don't delink hardlinks
  version-control t            ; Use version numbers on backups
  delete-old-versions t        ; Auto delete excess backups
  kept-new-versions 3          ; How many of newest versions to keep
  kept-old-versions 1          ; How many of old versions to keep
  )

(setq default-frame-alist '((fullscreen . maximized)       ; start maximized
                             (alpha-background . 100)))    ; transparency

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

;; Consider built-in packages when updating/installing packages
(setq package-install-upgrade-built-in t)

;; Auto focus new window
;; source: reddit.com/r/emacs/comments/aepvwq/how_to_automatically_switch_focus_to_newly/edsvalc
(require 'cl-lib)
(defvar my-display-buffers-no-select '("*Completions*"))
(define-advice display-buffer (:around (f &rest args) select-window)
  (let ((w (apply f args)))
    (when (and (windowp w)
            (not (cl-find-if
                   #'get-buffer-window my-display-buffers-no-select)))
      (select-window w))
    w))

;; Put buffer path into the title bar
(setq frame-title-format
  '(buffer-file-name "%b - %f"
     (dired-directory dired-directory
       (revert-buffer-function "%b"
         ("%b - Dir: " default-directory)))))

;; Use exec-path-from-shell package so Emacs env vars look the same as in shell
;; Helpful for setting up LSP
(use-package exec-path-from-shell
  :ensure t)
;; Set $PATH and exec-path from shell when in GUI frame Emacs
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Revert buffers when the underlying file has changed
;; global-auto-revert-mode was finicky, so just explicitly hooking
;; auto-revert-mode to several specific modes
(setopt auto-revert-avoid-polling nil)
(setopt auto-revert-interval 2)
(setopt auto-revert-check-vc-info t)
(dolist (hook '(dired-mode-hook magit-mode-hook ibuffer-mode-hook))
  (add-hook hook 'auto-revert-mode))

;; Right click gives context menu
(when (display-graphic-p)
  (context-menu-mode))

;; Fill-column indicator
(setq-default fill-column 81)  ; Set number of columns to use
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Turn off indent tab mode
(setq-default indent-tabs-mode nil)
;; Use common lisp indenting
(setq lisp-indent-function 'common-lisp-indent-function)
(setq lisp-indent-offset 2)

;; Default sorting order in ibuffer. Cycle with , key
(setq ibuffer-default-sorting-mode 'filename/process)

;; HideShow minor mode for code folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

(use-package adaptive-wrap
  :ensure t
  :config
  ;; Make adaptive wrap mode global (no built-in global mode as far as I know)
  ;; Useful in conjunction with visual-line mode
  (progn
    (setq-default adaptive-wrap-extra-indent 1)
    (defun turn-on-adaptive-wrap-prefix-mode ()
      "Turns on adaptive-wrap-prefix-mode."
      (interactive)
      (adaptive-wrap-prefix-mode 1))
    (define-globalized-minor-mode global-adaptive-wrap-prefix-mode
      adaptive-wrap-prefix-mode
      turn-on-adaptive-wrap-prefix-mode)
    (global-adaptive-wrap-prefix-mode 1)))

;; Jinx - spell-checker
(use-package jinx
  :ensure t
  :bind (("M-$" . jinx-correct)
	  ("M-n" . jinx-next)
	  ("M-p" . jinx-previous)
	  ("C-M-$" . jinx-languages)))
(global-jinx-mode)

;; Some tweaks
(setq visible-bell t)                  ; Flash on bell ring
(pixel-scroll-precision-mode 1)        ; Smoother scrolling
(global-display-line-numbers-mode 1)   ; Show line numbers on side
(transient-mark-mode 1)                ; Transient mark mode
(global-visual-line-mode 1)            ; Soft wrap text at buffer edge
(desktop-save-mode 1)                  ; Restore session
(save-place-mode 1)                    ; Save place when last closed
(savehist-mode 1)                      ; Enable saving of minibuffer history
(icomplete-mode 1)                     ; Enable icomplete mode
(recentf-mode 1)                       ; M-x recentf-open-files
(electric-pair-mode 1)                 ; Enable electric pair mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD MODULES & CUSTOM PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-bindings)
(require 'init-dired)
(require 'init-lsp)
(require 'init-minibuffer-and-completions)
(require 'init-mode-line)
(require 'init-org)
(require 'init-prog)
(require 'init-theme)
