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
;; use envrc too, TODO work needed here, to manual for now
;; check out micromamba.el too

(setenv "PATH" (concat "/Users/irubachev/micromamba/envs/tabr/bin:" "/Users/irubachev/.nix-profile/bin:" "/nix/var/nix/profiles/default/bin:" (getenv "PATH")))
(setq exec-path (append '("/Users/irubachev/micromamba/envs/tabr/bin" "/Users/irubachev/.nix-profile/bin" "/nix/var/nix/profiles/default/bin") exec-path))
(setq safe-local-variable-values '((eval setenv "PYTHONPATH" "/Users/irubachev/repos/p")))
(setq load-path (cons (concat user-emacs-directory "lisp") load-path))


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

;; My functions

(defun +beginning-of-line (arg)
 "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
 (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  ;; Begin of line or back to indent
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line] '+beginning-of-line)

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
(load-theme 'modus-vivendi :no-confirm)

;; Completion

(global-corfu-mode 1) ;; in buffer drop-down menu
(vertico-mode 1)      ;; vertical completion for everything
(marginalia-mode 1)

(setq corfu-auto t)
(setq corfu-auto-prefix 2)
(setq corfu-auto-delay 0.1)

(setq vertico-resize nil)
(setq vertico-cycle t)
(setq vertico-count 17)

;; TODO What is this?

;; (defun crm-indicator (args)
;;   (cons (format "[CRM%s] %s"
;;                 (replace-regexp-in-string
;;                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
;;                  crm-separator)
;;                 (car args))
;;         (cdr args)))
;; (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
(keymap-set vertico-map "DEL" #'vertico-directory-delete-char)

;; TODO fixed in emacs 30 (https://github.com/minad/vertico#tramp-hostname-and-username-completion)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion))))

(setq marginalia-annotators
      '(marginalia-annotators-heavy
        marginalia-annotators-light
        nil))


;; IDE settings
(add-hook 'python-mode-hook #'eglot-ensure)
(setq python-indent-def-block-scale 1)

(require 'treesit)

(defun p-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((python "https://github.com/tree-sitter/tree-sitter-python")
               ;; add more grammars
               ))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

(p-setup-install-grammars)
;; (dolist (mapping '((python-mode . python-ts-mode)
;;                    ;; TODO add more grammars
;;                    ))
;;   (add-to-list 'major-mode-remap-alist mapping))





;; Optional, but recommended. Tree-sitter enabled major modes are
;; distinct from their ordinary counterparts.
;;
;; You can remap major modes with `major-mode-remap-alist'. Note
;; that this does *not* extend to hooks! Make sure you migrate them
;; also




;; TODO find a way to use ruff with emacs
;;(setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
;; (defun p-setup-python-linting ()
;;   ;; (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend) 
;;   (add-hook 'flymake-diagnostic-functions 'python-flymake))
;; (add-hook 'eglot-managed-mode-hook #'p-setup-python-linting)


(setq-default project-vc-ignores '("./exp"))  ;; for my particular use-case

;; org-roam
(setq org-roam-directory (file-truename "~/org"))
(setq org-return-follows-link t)

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-tempo t)
  (org-roam-db-autosync-mode))

;; Optimize the agenda setup
(require 'org-roam-optimize-agenda)

;; kick-start the above module with this: 

;; (dolist (file (org-roam-list-files))
;;   (message "processing %s" file)
;;   (with-current-buffer (or (find-buffer-visiting file)
;;                            (find-file-noselect file))
;;     (vulpea-project-update-tag)
;;     (save-buffer)))

;; TODOs, see the emacs-configuration.org for tasks

;; Profile emacs startup

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval setenv "PYTHONPATH" "/Users/irubachev/repos/big")
     (eval setenv "PYTHONPATH" "/Users/irubachev/repos/p"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
