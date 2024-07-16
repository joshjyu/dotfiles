;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THEMING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default font
(set-face-attribute 'default nil :font "Iosevka Comfy-11")
;; Default line spacing is 0
(setq-default line-spacing 0.1)

;; Spacious padding to modify various borders and lines
(use-package spacious-padding
  :ensure t)
(setq spacious-padding-widths
  '(:internal-border-width 15
     :header-line-width 4
     :mode-line-width 3
     :tab-width 4
     :right-divider-width 30
     :scroll-bar-width 8
     :fringe-width 8))
(spacious-padding-mode 1)

;; MODUS THEME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package modus-themes
  :ensure t)
(require-theme 'modus-themes)

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
  (interactive)
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
  (modus-themes-load-theme 'modus-operandi))

;; Customized Modus Vivendi
(defun my-custom-modus-vivendi ()
  (interactive)
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
  (modus-themes-load-theme 'modus-vivendi))

;; Disable any other themes just in case
(mapc #'disable-theme custom-enabled-themes)

;; Desktop-save-mode also saves certain theme elements so when it saves
;; a session in dark themes (modus vivendi for example), certain elements get
;; messed up when loading a custom theme at startup. Hence load custom theme 
;; after the desktop file is read so it loads it in cleanly.
(add-hook 'desktop-after-read-hook #'my-custom-modus-operandi)

(provide 'init-theme)
