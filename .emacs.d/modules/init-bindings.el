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
(global-set-key (kbd "C-c m") 'my-custom-modus-themes-toggle)

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

;; HideShow global bindings
(global-set-key (kbd "C-c f") 'hs-hide-block)
(global-set-key (kbd "C-c s") 'hs-show-block)

;; Company - activate manually
;; Important to use TAB instead of <tab> as the latter interferes with
;; minibuffer completion-at-point tab
(global-set-key (kbd "TAB") 'company-complete)

(provide 'init-bindings)
