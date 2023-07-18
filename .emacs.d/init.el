;; -*- lexical-binding: t; -*-

;; Defaults

(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode '(5 . 0))

(delete-selection-mode)

(setq ring-bell-function 'ignore
      inhibit-startup-message t
      sentence-end-double-space nil
    
      dired-auto-revert-buffer t
      global-auto-revert-mode t
      global-auto-revert-non-file-buffers t

      gc-cons-threshold most-positive-fixnum)


(setq find-file-visit-truename t)


;; modules

(require 'nix-mode)


