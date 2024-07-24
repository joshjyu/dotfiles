;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BASIC CHANGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add modules path
(add-to-list 'load-path (concat user-emacs-directory
                          (convert-standard-filename "modules/")))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

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

;; Consider built-in packages when updating/installing packages
(setq package-install-upgrade-built-in t)

;; Auto focus new window
;; see: reddit.com/r/emacs/comments/aepvwq/how_to_automatically_switch_focus_to_newly/edsvalc
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
;; Helpful for setting up LSP in particular
(use-package exec-path-from-shell
  :ensure t
  :config
  ;; Set $PATH and exec-path from shell when in GUI frame Emacs
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Revert buffers when the underlying file has changed
;; global-auto-revert-mode was finicky, so just explicitly hooking
;; auto-revert-mode to several specific modes
(setopt auto-revert-avoid-polling nil)
(setopt auto-revert-interval 2)
(setopt auto-revert-check-vc-info t)
(dolist (hook
          '(dired-mode-hook
             dired-sidebar-mode-hook
             ibuffer-sidebar-mode
             magit-mode-hook
             ibuffer-mode-hook))
  (add-hook hook 'auto-revert-mode))

;; Right click gives context menu
(when (display-graphic-p)
  (context-menu-mode))

;; Turn off indent tab mode
(setq-default indent-tabs-mode nil)
;; Use common lisp indenting
(setq lisp-indent-function 'common-lisp-indent-function)
(setq lisp-indent-offset 2)

;; HideShow minor mode for code folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Fill-column indicator
(setq-default fill-column 81)  ; Set number of columns to use
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

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
	  ("C-M-$" . jinx-languages))
  :config (global-jinx-mode))

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
;;; MODE-LINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default mode-line-percent-position nil)
(line-number-mode)
(column-number-mode)

;; Change "git-" or "git:" in vc indicator to all-the-icons git glyph
;; C-h v all-the-icons-data/* for data alist
(setcdr (assq 'vc-mode mode-line-format)
  '((:eval (replace-regexp-in-string
             "^ Git." " " vc-mode))))

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
  :ensure t
  :config
  ;; use-package has its own special :delight keyword but I prefer having all
  ;; config in one spot here
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
              (lsp-lens-mode nil lsp-lens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MINIBUFFER/COMPLETION PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico to enable vertical view in minibuffer
(use-package vertico
  :ensure t
  :config
  (setq
    vertico-resize t
    vertico-count 20)
  (vertico-mode))

;; Marginalia to enable annotations next to entries in the minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Consult - note the global bindings in global keybindings section
(use-package consult
  :ensure t)

;; Orderless - orderless completion style
(use-package orderless
  :ensure t
  :after minibuffer 
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Which-key to display keybinding completion option
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Company - text completion framework package
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
          ;; Unbind RET from selecting completions, instead use TAB for that
          ("<return>" . nil)
          ("RET" . nil)
          ("<tab>" . 'company-complete-selection))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SIDEBAR, DIRED, IBUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired
  :config
  ;; Do not create new buffers for every directory
  (setf dired-kill-when-opening-new-dired-buffer t)
  ;; Copy and delete recursively into directories
  (setq dired-recursive-copies 'always
    dired-recursive-deletes 'always
    delete-by-moving-to-trash t))

;; Add icons in dired mode
(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)
          (dired-mode . dired-hide-details-mode)))

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
      ("Magit" (or (mode . magit-mode)
                 (derived-mode . magit-mode)))
      ("Proc" (process))
      ("Emacs" (filename . ".emacs.d*"))
      ("Org" (or (file-extension . "org")
               (derived-mode . org-mode)
               (derived-mode . org-agenda-mode)))
      ("Prog" (or (derived-mode . prog-mode)
                (derived-mode . mhtml-mode)
                (derived-mode . gfm-mode)))
      )))

;; Tell ibuffer and ibuffer-sidebar to load the filter group automatically
(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "MyList")))

;; Put [Default] filter group at the top
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

;; Toggle both dired and ibuffer sidebars simultaneously
;; Global keybinding set to C-x C-n
(defun +sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (ibuffer-sidebar-toggle-sidebar)
  (dired-sidebar-toggle-sidebar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TREESITTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Treesit-auto might eventually be obsolete as Emacs updates native
;; treesitter in future releases
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt) ; Prompt when installing grammars
  :config
  ;; Add grammars to auto-mode-alist automatically
  (treesit-auto-add-to-auto-mode-alist 'all)
  (setopt treesit-font-lock-level '4)
  (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YASNIPPET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :ensure t
  :bind
  ;; Also accommodate my company keybindings by disabling tab
  (:map yas-minor-mode-map
    ("C-<tab>" . yas-expand)
    ("<tab>" . nil)
    ("TAB" . nil))
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)
;; Custom snippets are in ~/.emacs.d/snippets

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :bind ("C-x g" . magit-status)
  :ensure t)

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
;;; PRETTIER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First Prettier has to be installed locally
;; prettier.io/docs/en/install
;; Need node on exec-path. Also prettier uses nvm
(use-package prettier
  :ensure t
  :bind
  ;; Lazy load prettier and invoke manually
  ("C-c C-p" . prettier-prettify))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JAVASCRIPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js
  :mode ("\\.js\\'")
  :config
  (setq js-indent-level 2))

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
    org-ellipsis "...")
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
       )))

;; Below line in case I want to add custom tags
;; (setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("@laptop" . ?l))

;; Org-journal mode
(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir org-directory
    org-journal-file-type 'monthly
    org-journal-file-format "%Y%m-journal.org"
    org-journal-time-format ""
    org-journal-hide-entries-p nil
    org-journal-carryover-items "TODO=\"TODO\"|TODO=\"LT-TODO\"|TODO=\"IDEA\"|TODO=\"MAYBE\"|TODO=\"IN-PROGRESS\"|TODO=\"WAITING\""))

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
  (setq treemacs-no-delete-other-windows nil
    treemacs-position 'right))

(use-package dap-mode
  :ensure t
  :custom
  (lsp-enable-dap-auto-configure t)
  :config
  ;; dap-firefox seems to have been broken for awhile
  ;; github.com/emacs-lsp/dap-mode/issues/547
  ;; Just use developer tools
  (require 'dap-node)
  (dap-node-setup)
  (dap-ui-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note to self: s = super, S = shift

;; Use TAB instead of C-M-i for completion-at-point
(setq tab-always-indent 'complete)

;; Windmove to easily switch windows
(global-set-key (kbd "s-j") 'windmove-left)
(global-set-key (kbd "s-k") 'windmove-down)
(global-set-key (kbd "s-l") 'windmove-up)
(global-set-key (kbd "s-;") 'windmove-right)

;; C-x C-m to toggle themes
(global-set-key (kbd "C-c m") 'my-custom-modus-themes-toggle)

;; C-x C-r to open recent files buffer
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;; C-x C-b to open ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; C-x C-n to +sidebar-toggle
(global-set-key (kbd "C-x C-n") '+sidebar-toggle)

;; Global org keybindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c j") #'org-journal-new-date-entry)

;; Global consult keybindings
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "M-s l") 'consult-line)

;; HideShow global bindings
(global-set-key (kbd "C-c f") 'hs-hide-block)
(global-set-key (kbd "C-c s") 'hs-show-block)

;; Company - activate manually
;; Important to use TAB instead of <tab> as the latter interferes with
;; minibuffer completion-at-point tab
(global-set-key (kbd "TAB") 'company-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THEMING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default font
(set-face-attribute 'default nil :font "Iosevka Comfy-11")
;; Default line spacing is 0
(setq-default line-spacing 0.1)

;; Icon package - important to also install the Resource Fonts
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
    '( :internal-border-width 5
       :header-line-width 4
       :mode-line-width 3
       :tab-width 4
       :right-divider-width 20
       :scroll-bar-width 8
       :fringe-width 8))
  (spacious-padding-mode))

;; MODUS THEME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package modus-themes
  :ensure t
  :config
  (require-theme 'modus-themes))

;; Fill-column-indicator customization in Modus themes
(modus-themes-with-colors
  (custom-set-faces
    `(fill-column-indicator
       ((,c :height 0.1 :background, bg-inactive :foreground ,bg-active)))))

;; Custom toggle function since using custom modus functions
(defun my-custom-modus-themes-toggle ()
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
    (my-custom-modus-vivendi)
    (my-custom-modus-operandi)))

;; Customized Modus Operandi
(defun my-custom-modus-operandi ()
  (setq modus-themes-common-palette-overrides
    '(
       ;; Colorful mode line
       (bg-mode-line-active "#c0d2f3")
       (bg-mode-line-inactive "#99a8c2")
       (fg-mode-line-active "#000000")
       (fg-mode-line-inactive "#4c4c4c")
       ;; Borderless mode line
       (border-mode-line-active unspecified)
       (border-mode-line-inactive unspecified)
       ;; Fringe color/invisible
       (fringe "#ffffff")
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
  (modus-themes-load-theme 'modus-operandi))

;; Customized Modus Vivendi
(defun my-custom-modus-vivendi ()
  (setq modus-themes-common-palette-overrides
    '(
       ;; Colorful mode line
       (bg-mode-line-active "#003366")
       (bg-mode-line-inactive "#001933")
       (fg-mode-line-active "#ffffff")
       (fb-mode-line-inactive "#7f7f7f")
       ;; Borderless mode line
       (border-mode-line-active unspecified)
       (border-mode-line-inactive unspecified)
       ;; Fringe color/invisible
       (fringe "#000000")
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
  (modus-themes-load-theme 'modus-vivendi))

;; Disable any other themes just in case
(mapc #'disable-theme custom-enabled-themes)

;; Desktop-save-mode also saves certain theme elements so when it saves
;; a session in dark themes (modus vivendi for example), certain elements get
;; messed up when loading a custom theme at startup. Hence load custom theme 
;; after the desktop file is read so it loads it in cleanly.
(add-hook 'desktop-after-read-hook #'my-custom-modus-operandi)
