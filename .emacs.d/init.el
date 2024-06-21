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
(setopt fill-column 100)

(setq inhibit-startup-message t)
(setq sentence-end-double-space nil)
(setq tab-always-indent 'complete)


;; TODO some updates needed here
(setq use-short-answer t)
(defalias 'yes-or-no-p 'y-or-n-p)

(when (display-graphic-p)
  (context-menu-mode))




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

(setenv "PATH" (concat "/Users/irubachev/micromamba/envs/tabred/bin:" "/Users/irubachev/.nix-profile/bin:" "/nix/var/nix/profiles/default/bin:" (getenv "PATH")))
(setq exec-path (append '("/Users/irubachev/micromamba/envs/tabred/bin" "/Users/irubachev/.nix-profile/bin" "/nix/var/nix/profiles/default/bin") exec-path))
(setq load-path (cons (concat user-emacs-directory "lisp") load-path))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(defun add-linter-ignore-comment ()
  "Add a linter ignore comment for the current line in a Python file."
  (interactive)
  (save-excursion
    (end-of-line)
    (delete-trailing-whitespace (line-beginning-position) (line-end-position))
    (insert "  # noqa")))

;; code, python, ignore
(global-set-key (kbd "C-c p i") 'add-linter-ignore-comment)

;; this is annoying
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<double-wheel-up>"))
(global-unset-key (kbd "C-<triple-wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<double-wheel-down>"))
(global-unset-key (kbd "C-<triple-wheel-down>"))
(global-unset-key (kbd "C-<mouse-4>"))
(global-unset-key (kbd "C-<mouse-5>"))

;; Buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; workspaces

(setopt tab-bar-show 1)
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)


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

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . bottom)
               (window-height . 0.2)))

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


(with-eval-after-load 'consult 
  (setq default-consult-ripgrep-args consult-ripgrep-args)
  (setopt consult-ripgrep-args (string-join (cons "~/.nix-profile/bin/rg" (cdr (string-split default-consult-ripgrep-args " "))) " ")))

;; embark actions
(global-set-key (kbd "C-.") 'embark-act)
(setq embark-prompter 'embark-completing-read-prompter)
(setq embark-indicators
      '(embark-highlight-indicator))

;; consult keybindings
(global-set-key (kbd "C-c c g") 'consult-ripgrep)
(global-set-key (kbd "C-c c f") 'consult-flymake)

(setq corfu-auto t)
(setq corfu-auto-prefix 2)
(setq corfu-auto-delay 0.1)

(setq vertico-resize nil)
(setq vertico-cycle t)
(setq vertico-count 17)

;; this is great in buffer search 
(ctrlf-mode +1)

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

;; better help
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

(which-key-mode)


;; add demos to help buffers
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;; IDE settingsx
;; (require 'lsp-pyright)
;; (require 'lsp-ruff-lsp)

(add-hook 'python-mode-hook 'eglot-ensure)


(setq flymake-no-changes-timeout 0.0)

(with-eval-after-load 'eglot
  ;; Looks like this does not work with stubs, thus pytorch completions don't work for examples -- bad for now. But cool server anyways
  ;; (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("/Users/irubachev/.nix-profile/bin/pylyzer" "--server")))
  (eglot-booster-mode)
)


(setopt lsp-pyright-typechecking-mode "basic")
(setq jupyter-repl-echo-eval-p t)
(setq native-comp-jit-compilation-deny-list '("jupyter.*.el"))

;; (with-eval-after-load 'lsp-mode
;;   (setq lsp-file-watch-ignored-directories (append lsp-file-watch-ignored-directories '("exp" "data" "cache"))))


;; TODO disable all diagnostics from pyright


;; (with-eval-after-load 'lsp-mode
;;   (setq lsp-diagnostic-filter 'my/filter-pyright)
;;   (lsp-defun my/filter-pyright ((params &as &PublishDiagnosticsParams :diagnostics)
;;                                 _workspace)
;;     (lsp:set-publish-diagnostics-params-diagnostics
;;      params
;;      (or (seq-filter (-lambda ((&Diagnostic :source?))
;;                        (not (string= "pyright" source?)))
;;                      diagnostics)
;;          []))
;;     params)
;;   )

;; (setq lsp-ruff-lsp-show-notifications "always")
;; (setq lsp-ruff-lsp-ruff-path "/Users/irubachev/.nix-profile/bin/ruff")
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

;; Home-backed remote development

;; todo improve this part

(defun +unison-sync ()
  (when (string-prefix-p "/Users/irubachev/repos" buffer-file-name)
    (save-window-excursion
      (ignore-errors
        (async-shell-command "unison big -ui text -auto -batch")))))

(setopt async-shell-command-buffer 'new-buffer)
(add-hook 'after-save-hook '+unison-sync)

;; this was fun
(require 'ssh-tunnels)


(setq ssh-tunnels-configurations
      '((:name "zomb-jupyter"
               :local-port 9999
               :remote-port 9999
               :login "3090")
        (:name "zomb-cv4"
               :local-port 10000
               :remote-port 10000
               :login "zomb-research-cv-4.zombie.yandex.net")))


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


;; AI

(gptel-make-ollama
 "hermes-7b-q5"
 :host "localhost:11434"
 :models '("hermes:latest")
 :stream t)

(gptel-make-ollama
 "deepseek-7b-q5"
 :host "localhost:11434"
 :models '("deepseek:latest")
 :stream t)

;; (setq jupyter--servers nil)
;; (jupyter-servers)

;; TODO find a way to use ruff with emacs
;;(setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
;; (defun p-setup-python-linting ()
;;   ;; (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend) 
;;   (add-hook 'flymake-diagnostic-functions 'python-flymake))
;; (add-hook 'eglot-managed-mode-hook #'p-setup-python-linting)

;; pdftools
(pdf-loader-install)

(setq-default project-vc-ignores '("**/exp" "exp" "./exp" "./archive"))  ;; for my particular use-case

;; org-roam
(setq org-roam-directory (file-truename "~/org"))
(setq org-return-follows-link t)
(setq org-startup-folded 'show2levels)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)")
        (sequence "|" "CANCELED(c)")))


(with-eval-after-load 'org
  (require 'org-roam-optimize-agenda)
  (require 'org-noter)
  (require 'org-pdftools)

  (org-pdftools-setup-link)
  (add-to-list 'org-modules 'org-tempo t)
  (org-roam-db-autosync-mode)
  )


(defun p-maximize-current-window ()
  "Maximize current window, make it occupy the whole screen"
  (interactive)
  (let ((frame (selected-frame))
        (pixel-width (display-pixel-width))
        (pixel-height (display-pixel-height)))
    (set-frame-size
     frame
     (- pixel-width 4)
     (- pixel-height 4)
     t)
    (message "%d" (% pixel-width 6))
    (set-frame-position frame 0 0)))

(global-set-key (kbd "C-x 5 m") 'p-maximize-current-window)

;; Optimize the agenda setup
(display-pixel-width)

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
   '((eval setenv "PROJECT_DIR" "/Users/irubachev/repos/diff_2")
     (eval setenv "PYTHONPATH" "/Users/irubachev/repos/diff_2")
     (eval setenv "PROJECT_DIR" "/Users/irubachev/repos/tabind")
     (eval setenv "PYTHONPATH" "/Users/irubachev/repos/tabind")
     (eval setenv "PROJECT_DIR" "/Users/irubachev/repos/ht")
     (eval setenv "PYTHONPATH" "/Users/irubachev/repos/ht")
     (eval setenv "PROJECT_DIR" "/Users/irubachev/repos/big")
     (eval setenv "PYTHONPATH" "/Users/irubachev/repos/big")
     (eval setenv "PYTHONPATH" "/Users/irubachev/repos/p"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
