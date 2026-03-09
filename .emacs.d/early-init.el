;; Frame config
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-resize-pixelwise t
  frame-inhibit-implied-resize t
  inhibit-splash-screen t
  inhibit-startup-screen t
  inhibit-startup-echo-area-message (user-login-name))
;; Set global variables for theme config
(defvar my-default-font "Iosevka Curly-12")
;; Light theme colors
(defvar my-background-color "#ffffff")
(defvar my-sidebar-bg-light "#f5f5f5")
(defvar my-foreground-color "#000000")
;; Dark theme colors
(defvar my-dark-bg-color "#141414")
(defvar my-sidebar-bg-dark "#0f0f0f")
(defvar my-dark-fg-color "#ffffff")
;; Set font early to prevent font changes during startup
(add-to-list 'default-frame-alist `(font . ,my-default-font))
;; Prevent white flash by setting background early
(add-to-list 'default-frame-alist `(background-color . ,my-dark-bg-color))
;; Set initial frame size to fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; plists provide better performance in lsp mode
(setenv "LSP_USE_PLISTS" "true")
;; Garbage collection threshold - default is 800000 bytes
;; Helpful info by Emacs maintainer: reddit.com/r/emacs/comments/yzb77m/an_easy_trick_i_found_to_improve_emacs_startup/iwz1vek/
(setq gc-cons-threshold 100000000)
;; Suppress less significant warnings
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq warning-minimum-level ':error)
