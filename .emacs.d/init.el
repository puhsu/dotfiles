;; -*- lexical-binding: t; -*-

;; Defaults

(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode '(0 . 0))

(delete-selection-mode)
(global-so-long-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)

(setq user-full-name "Ivan Rubachev")
(setq user-mail-address "irubachev@gmail.com")

(setq frame-title-format '("%b"))
(setq frame-resize-pixelwise t)

(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t)
(setq sentence-end-double-space nil)
(setq tab-always-indent 'complete)

;; TODO some updates needed here
(setq use-short-answer t)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil)    ;; stop creating backup~ files
(setq auto-save-default nil)    ;; stop creating #autosave# files
(setq create-lockfiles nil)     ;; stop creating .# files
(setq require-final-newline t)
 
(setq dired-auto-revert-buffer t)
(setq find-file-visit-truename t)
(setq global-auto-revert-non-file-buffers t)
(setq bookmark-save-flag 1) ;; TODO - readup on bookmarks

(setq gc-cons-threshold most-positive-fixnum)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

;; TODO investigate further but good enough for now
(set-display-table-slot standard-display-table 'truncation 32)

(pixel-scroll-precision-mode)
(setq pixel-scroll-precision-large-scroll-height 20.0)
(setq pixel-scroll-precision-use-momentum t)
(setq scroll-conservatively 101)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)
(setq fast-but-imprecise-scrolling t)

;; fixup the PATH environment variableTODO (make it load a file
;; instead, link with home-manager somehow)

;; (list exec-path)
;; exec-path
;; (setenv "PATH" (concat "/Users/irubachev/.nix-profile/bin:" "/nix/var/nix/profiles/default/bin:" (getenv "PATH")))
(setq exec-path (append '("/Users/irubachev/.nix-profile/bin" "/nix/var/nix/profiles/default/bin") exec-path))




(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)


;; Window configuration for special windows.
;; This section inspired by the article "Demystifying Emacs’s Window
;; Manager" found here:
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;; TODO read, understand how it works and organize

(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

;; Show dictionary definition on the left
(add-to-list 'display-buffer-alist
             '("^\\*Dictionary\\*"
               (display-buffer-in-side-window)
               (side . left)
               (window-width . 70)))


;; Modules

;; UI stuff

(require 'modus-themes)

(add-to-list 'default-frame-alist '(font . "PragmataPro Mono Liga 13"))

(defun p-enable-line-numbers ()
  "Wrapper around display-line-numbers-mode that disables line numbers"
  (display-line-numbers-mode 1))

(dolist (mode '(conf-mode prog-mode))
  (add-hook (intern (format "%s-hook" mode)) #'p-enable-line-numbers))

(setq display-line-numbers-grow-only t)

;; TODO theme customizations
;; - no bold in line numbers (or make all lines higher)

(load-theme 'modus-vivendi :no-confirm)

;; Completion

(global-corfu-mode 1) ;; in buffer drop-down menu
(vertico-mode 1)      ;; vertical completion for everything
(marginalia-mode 1)

(setq vertico-resize nil)
(setq vertico-cycle t)
(setq vertico-count 17)

;; TODO What is this?

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(keymap-set vertico-map "DEL" #'vertico-directory-delete-char)


(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(setq marginalia-annotators
      '(marginalia-annotators-heavy
        marginalia-annotators-light
        nil))


;; IDE settings
(add-hook 'python-mode-hook #'eglot-ensure)
(setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))

(defun p-setup-python-linting ()
  ;; (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend) 
  (add-hook 'flymake-diagnostic-functions 'python-flymake))

(add-hook 'eglot-managed-mode-hook #'p-setup-python-linting)


(setq-default project-vc-ignores '("./exp"))  ;; for my particular use-case


;; TODO
;; - convinience function to resize buffer 



;; Profile emacs startup

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))


