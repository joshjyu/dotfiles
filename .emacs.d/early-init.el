;; Garbage collection threshold - default is 800000 bytes
;; Helpful info by Emacs maintainer: reddit.com/r/emacs/comments/yzb77m/an_easy_trick_i_found_to_improve_emacs_startup/iwz1vek/
(setq gc-cons-threshold 4000000)
;; Suppress insignificant warnings
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq warning-minimum-level ':error)
;; Frame config
(setq frame-resize-pixelwise t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name))

(menu-bar-mode -1)                   ; Hide menu bar
(tool-bar-mode -1)                   ; Hide tool bar
(scroll-bar-mode -1)                 ; Hide scroll bar
