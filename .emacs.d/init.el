;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BASIC CHANGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Separate custom.el file for customization clutter
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Add modules path
(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "modules/")))

;; Save ~ files in a backup directory (instead of buffer's dir)
(setq backup-directory-alist '(("." . "~/.ebackup/"))
      backup-by-copying t          ; Don't delink hardlinks
      version-control t            ; Use version numbers on backups
      delete-old-versions t        ; Auto delete excess backups
      kept-new-versions 3          ; How many of newest versions to keep
      kept-old-versions 1          ; How many of old versions to keep
      )

(setq default-frame-alist '((fullscreen . maximized)       ; start maximized
                            (alpha-background . 100)))     ; transparency

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize t)

;; Use exec-path-from-shell package so Emacs env vars look the same as in shell
(use-package exec-path-from-shell
  :ensure t)
;; Set $PATH and exec-path from shell when in GUI frame Emacs
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Consider built-in packages when updating/installing packages
(setq package-install-upgrade-built-in t)

;; Revert buffers when the underlying file has changed
(setopt auto-revert-avoid-polling nil)
(setopt auto-revert-interval 2)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Make adaptive wrap mode global (no built-in global mode afaik)
;; Useful in conjunction with visual-line mode
(use-package adaptive-wrap
  :ensure t
  :config
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
      '(buffer-file-name "%b - %f"                ; File buffer
        (dired-directory dired-directory          ; Dired buffer
         (revert-buffer-function "%b"             ; Buffer Menu
          ("%b - Dir: " default-directory)))))    ; Plain buffer

;; Turn off indent tab mode by default
(setq-default indent-tabs-mode nil)

;; Right click gives context menu
(when (display-graphic-p)
  (context-menu-mode))

;; Fill-column indicator
(setq-default fill-column 81)  ; Set number of columns to use
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Default sorting order in ibuffer. Cycle with , key
(setq ibuffer-default-sorting-mode 'filename/process)

;; Some tweaks
(setq visible-bell t)                  ; Flash on bell ring
(pixel-scroll-precision-mode 1)        ; Smoother scrolling
(global-display-line-numbers-mode 1)   ; Show line numbers on side
(transient-mark-mode 1)                ; Transient mark mode
(column-number-mode 1)                 ; Column number in mode line
(global-visual-line-mode 1)            ; Soft wrap text at buffer edge
(desktop-save-mode 1)                  ; Restore session
(save-place-mode 1)                    ; Save place when last closed
(savehist-mode 1)                      ; Enable saving of minibuffer history
(icomplete-mode 1)                     ; Enable icomplete mode
(recentf-mode 1)                       ; M-x recentf-open-files
(electric-pair-mode 1)                 ; Enable electric pair mode

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
  (setq company-idle-delay nil
        ;; Manually activate with <tab> (see global bindings)
        company-minimum-prefix-length 1
        ;; Results are more sensible when preferring same case prefix
        company-transformers '(company-sort-prefer-same-case-prefix)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TREESITTER & LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
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
  (lsp-completion-provider :none)
  (lsp-keep-workspace-alive nil)
  (lsp-log-io nil)
  (lsp-auto-configure t)
  (lsp-enable-indentation nil)
  (lsp-apply-edits-after-file-operations nil))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :bind (("C-c h" . lsp-ui-doc-focus-frame)
         ("C-c q" . lsp-ui-doc-hide))
  ;; To show doc at point: C-c p h g
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions nil
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YASNIPPET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Needed for company-capf to work correctly
(use-package yasnippet
  :ensure t
  :bind
  (:map yas-minor-mode-map
        ("C-<tab>" . yas-expand)
        ("<tab>" . nil)
        ("TAB" . nil)))
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MARKDOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JINX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package jinx
  :ensure t
  ;; Jinx mode-specific keybindings
  :bind (("M-$" . jinx-correct)
	 ("M-n" . jinx-next)
	 ("M-p" . jinx-previous)
	 ("C-M-$" . jinx-languages)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DIRED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add icons in dired mode
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :ensure t)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Hide details in dired mode to clean it up
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; Do not create new buffers for every directory
(setf dired-kill-when-opening-new-dired-buffer t)
;; Copy and delete recursively into directories
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ORG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure t
  :config
  (setq org-directory "~/Documents/synced/org/"
	org-agenda-files (list org-directory)
	org-todo-keywords '((sequence "TODO" "LT-TODO" "IDEA" "MAYBE" "IN-PROGRESS" "WAITING" "CANCELED" "DONE"))
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
	org-journal-carryover-items "TODO=\"TODO\"|TODO=\"LT-TODO\"|TODO=\"IDEA\"|TODO=\"MAYBE\"|TODO=\"IN-PROGRESS\"|TODO=\"WAITING\""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THEMING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default font
(set-face-attribute 'default nil :font "Iosevka Comfy-11")
;; Default line spacing is 0
(setq-default line-spacing 0.1)

;; Spacious padding to modify various borders and lines
(use-package spacious-padding
  :ensure t)
(setq spacious-padding-widths
      '( :internal-border-width 5
         :header-line-width 4
         :mode-line-width 0
         :tab-width 4
         :right-divider-width 0
         :scroll-bar-width 8
         :fringe-width 0))
(spacious-padding-mode 1)

;; MODUS THEME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package modus-themes
  :ensure t)
(require-theme 'modus-themes)

;; Disable any other themes just in case
(mapc #'disable-theme custom-enabled-themes)

;; Toggle between themes (see keybinding section)
(setq modus-themes-to-toggle '(modus-operandi modus-vivendi))

;; Fill-column-indicator customization in Modus
(modus-themes-with-colors
  (custom-set-faces
   `(fill-column-indicator ((,c :height 0.1 :background, bg-inactive :foreground ,bg-active)))))

;; Modus theme customizations
(setq modus-themes-common-palette-overrides
;; Borderless mode line
      '((border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)
;; Colorful mode line
	(bg-mode-line-active "#c0d2f3")
	(bg-mode-line-inactive "#99a8c2")
        (fg-mode-line-active "#000000")
	(fg-mode-line-inactive "#4c4c4c")
;; Invisible fringe 
	(fringe unspecified)
;; Line numbers customization
	(fg-line-number-inactive "gray50")
        (fg-line-number-active fg-main)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)
;; Colorful prompts
	(fg-prompt cyan)
	(bg-prompt bg-cyan-nuanced)
;; Colorful completion matches
	(fg-completion-match-0 blue)
        (fg-completion-match-1 magenta-warmer)
        (fg-completion-match-2 cyan)
        (fg-completion-match-3 red)
        (bg-completion-match-0 bg-blue-nuanced)
        (bg-completion-match-1 bg-magenta-nuanced)
        (bg-completion-match-2 bg-cyan-nuanced)
        (bg-completion-match-3 bg-red-nuanced)
;; Yellow comments, green strings
	(comment yellow-cooler)
        (string green-cooler)
;; More colorful headings
	(fg-heading-7 "#666666")
	(fg-heading-6 "#999999")
	(fg-heading-5 "#a68f7a")
        (fg-heading-4 "#805f41")
        (fg-heading-3 "#a94c4c")
	(fg-heading-2 "#477bc3")
	(fg-heading-1 "#668a59")
;; More colorful org blocks
	(bg-prose-block-contents bg-blue-nuanced)
        (bg-prose-block-delimiter bg-lavender)
        (fg-prose-block-delimiter fg-main)
;; More colorful org agenda
	(date-common cyan)             ; For timestamps and more
        (date-deadline red-warmer)
        (date-event magenta-warmer)
        (date-holiday blue)            ; For M-x calendar
        (date-now yellow-warmer)
        (date-scheduled magenta-cooler)
        (date-weekday cyan-cooler)
        (date-weekend blue-faint)
;; Language underlines less colorful
	(underline-err red-faint)
        (underline-warning yellow-faint)
        (underline-note cyan-faint)))

;; Packages are initialized after processing the init file
;; So need to load theme after init.el is processed
(add-hook 'after-init-hook
	  (lambda ()
	    (load-theme 'modus-operandi :no-confirm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note to self: s = super, S = shift

;; Use TAB instead of C-M-i for completion-at-point
(setq tab-always-indent 'complete)

;; Windmove to easily switch windows
(global-set-key (kbd "s-u") 'windmove-left)
(global-set-key (kbd "s-i") 'windmove-down)
(global-set-key (kbd "s-o") 'windmove-up)
(global-set-key (kbd "s-p") 'windmove-right)

;; C-x C-m to toggle themes
(global-set-key (kbd "C-x C-m") 'modus-themes-toggle)

;; C-x C-r to open recent files buffer
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; C-x C-b to open ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Global org keybindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c j") #'org-journal-new-date-entry)

;; Global consult keybindings
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "M-s l") 'consult-line)

;; Company - activate manually
;; Important to use TAB instead of <tab> as the latter interferes with
;; minibuffer completion-at-point tab
(global-set-key (kbd "TAB") 'company-complete)
