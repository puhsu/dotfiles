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

(when (display-graphic-p)
  (context-menu-mode))


(setq make-backup-files nil)    ;; stop creating backup~ files
(setq auto-save-default nil)    ;; stop creating #autosave# files
(setq create-lockfiles nil)     ;; stop creating .# files
(setq require-final-newline t)
 
(setq dired-auto-revert-buffer t)
(setq dired-dwim-target t)
(setq find-file-visit-truename t)
(setq global-auto-revert-non-file-buffers t)
(setq bookmark-save-flag 1) ;; TODO - readup on bookmarks

(setq gc-cons-threshold most-positive-fixnum)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

;; TODO investigate further but good enough for now
(set-display-table-slot standard-display-table 'truncation 32)

(setq scroll-conservatively 101)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)
(setq pixel-scroll-precision-use-momentum t)
(setq pixel-scroll-precision-momentum-min-velocity 1)
(setq pixel-scroll-precision-momentum-seconds 1000)
(ultra-scroll-mode 1)

;; fixup the PATH environment variableTODO (make it load a file
;; instead, link with home-manager somehow)
;; use envrc too, TODO work needed here, to manual for now
;; check out micromamba.el too

(setenv "PATH" (concat "/Users/irubachev/.nix-profile/bin:" "/nix/var/nix/profiles/default/bin:" (getenv "PATH")))
(setq exec-path (append '("/Users/irubachev/.nix-profile/bin" "/nix/var/nix/profiles/default/bin") exec-path))
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


(add-to-list 'default-frame-alist '(font . "PragmataPro Mono Liga 13"))

(defun p-enable-line-numbers ()
  "Wrapper around display-line-numbers-mode that disables line numbers"
  (display-line-numbers-mode 1))

(dolist (mode '(conf-mode prog-mode))
  (add-hook (intern (format "%s-hook" mode)) #'p-enable-line-numbers))

(setq display-line-numbers-grow-only t)

(require 'modus-themes)
(load-theme 'modus-operandi :no-confirm)

(require 'spacious-padding)

;; These are the default values, but I keep them here for visibility.
(setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))

;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
(setq spacious-padding-subtle-mode-line
      `( :mode-line-active 'default
         :mode-line-inactive vertical-border))

(spacious-padding-mode 1)

;; Cleaner .emacs.d

(require 'no-littering)

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))


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
;; (setq corfu-quit-at-boundary t)
;; (setq corfu-quit-no-match t)
;; (setq corfu-preselect 'prompt)


(setq vertico-resize nil)
(setq vertico-cycle t)
(setq vertico-count 17)

;; this is great in buffer search 
(ctrlf-mode +1)

;; same but for the search and replace in a buffer
(require 'visual-replace)
(visual-replace-global-mode 1)


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
(add-hook 'zig-mode-hook 'eglot-ensure)

;; (advice-add 'eldoc--echo-area-render :override
;;             (lambda    (docs)
;;               "Similar to `eldoc--format-doc-buffer', but for echo area.
;; Helper for `eldoc-display-in-echo-area'."
;;               (cl-loop for (item . rest) on docs
;;                        for (this-doc . plist) = item
;;                        for echo = (plist-get plist :echo)
;;                        for thing = (plist-get plist :thing)
;;                        unless (eq echo 'skip) do
;;                        (setq this-doc
;;                              (cond ((integerp echo) this-doc)
;;                                    ((stringp echo) echo)
;;                                    (t this-doc)))
;;                        (when thing (setq this-doc
;;                                          (concat
;;                                           (propertize (format "%s" thing)
;;                                                       'face (plist-get plist :face))
;;                                           ": "
;;                                           this-doc)))
;;                        (insert this-doc)
;;                        (when rest (insert "\n")))))

(setq flymake-no-changes-timeout 0.0)

;; (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))


;; Spell checking
(dolist (hook '(text-mode-hook))
  (add-hook hook #'jinx-mode))


(setopt lsp-pyright-typechecking-mode "basic")
(setq jupyter-repl-echo-eval-p t)
(setq native-comp-jit-compilation-deny-list '("jupyter.*.el"))

(advice-remove 'org-babel-do-load-languages #'ignore)

(with-eval-after-load 'org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (julia . t)
     (python . t)
     (jupyter . t))))



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
        (async-shell-command "unison big -ui text -auto -batch -silent")))))

(setopt async-shell-command-buffer 'new-buffer)
(add-hook 'after-save-hook '+unison-sync)

;; AI
;; gptel

(require 'gptel)
(require 'gptel-rewrite)
(require 'phsu-secrets)


(setq gptel-deepseek-backend
      (apply #'gptel-make-openai "deepseek"
             (plist-get gptel-backend-configs :deepseek)))

(setq gptel-anthropic-backend
      (apply #'gptel-make-anthropic "anthropic"
             (plist-get gptel-backend-configs :anthropic)))

(setq gptel-backend gptel-anthropic-backend)
(setq gptel-model 'claude-3-5-sonnet-20241022)

(add-hook 'gptel-post-response-functions 'gptel-end-of-response)

(defun gptel--rewrite-inline-diff (&optional ovs)
  "Start an inline-diff session on OVS."
  (interactive (list (gptel--rewrite-overlay-at)))
  (unless (require 'inline-diff nil t)
    (user-error "Inline diffs require the inline-diff package."))
  (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
              ((buffer-live-p ov-buf)))
    (with-current-buffer ov-buf
      (cl-loop for ov in (ensure-list ovs)
               for ov-beg = (overlay-start ov)
               for ov-end = (overlay-end ov)
               for response = (overlay-get ov 'gptel-rewrite)
               do (delete-overlay ov)
               (inline-diff-words
                ov-beg ov-end response)))))

(when (boundp 'gptel--rewrite-dispatch-actions)
  (add-to-list
   'gptel--rewrite-dispatch-actions '(?i "inline-diff")
   'append))

(define-key gptel-rewrite-actions-map (kbd "C-c C-i") 'gptel--rewrite-inline-diff)

;; (setq jupyter--servers nil)
;; (jupyter-servers)

;; TODO find a way to use ruff with emacs
;;(setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
;; (defun p-setup-python-linting ()
;;   ;; (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend) 
;;   (add-hook 'flymake-diagnostic-functions 'python-flymake))
;; (add-hook 'eglot-managed-mode-hook #'p-setup-python-linting)


(setq-default project-vc-ignores '("**/exp" "exp" "./exp" "./archive"))  ;; for my particular use-case

;; org-mode and text mode tweaks

(setopt fill-column 120)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'visual-fill-column-mode)

;; denote 
(require 'denote)
(setq denote-directory (expand-file-name "~/org"))
(setq denote-save-buffers t)

;; temporary for migratin org-roam files to denote
(defun phsu-roam-to-denote ()
  "Migrate current org file to denote format.
Works on demand for any org file, extracting title and tags from its contents."
  (interactive)
  (when buffer-file-name
    (let* ((title (or (org-get-title)
                      (file-name-base buffer-file-name)))
           (filetags (org-get-filetags))
           (denote-rename-no-confirm t))
      (when (yes-or-no-p 
             (format "Migrate '%s' to denote format?" title))
        ;; Remove PROPERTIES drawer
        (org-with-wide-buffer
         (goto-char (point-min))
         (when (re-search-forward ":PROPERTIES:\n\\([^\0]*?\\):END:\n" nil t)
           (replace-match "")))
        (save-buffer)
        (denote-rename-file buffer-file-name title filetags)))))

(defun org-get-title ()
  "Get the title from current org file."
  (org-element-map (org-element-parse-buffer) 'keyword
    (lambda (kw)
      (when (string= (org-element-property :key kw) "TITLE")
        (org-element-property :value kw)))
    nil t))

(defun org-get-filetags ()
  "Get FILETAGS from current org file."
  (let ((filetags (org-element-map (org-element-parse-buffer) 'keyword
                   (lambda (kw)
                     (when (string= (org-element-property :key kw) "FILETAGS")
                       (org-element-property :value kw)))
                   nil t)))
    (when filetags
      (split-string (replace-regexp-in-string "[:]+" " " filetags) " " t))))

(setq org-src-preserve-indentation t)
(setq org-return-follows-link t)
(setq org-startup-folded 'show2levels)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)")
        (sequence "|" "CANCELED(c)")))

(with-eval-after-load 'org
  (require 'optimize-agenda)
  (add-to-list 'org-modules 'org-tempo t))


(defun p-maximize-current-window ()
  "Maximize current window, make it occupy the whole screen"
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-parameter frame 'fullscreen 'maximized)
    (set-frame-position frame 0 0)))

(global-set-key (kbd "C-x 5 m") 'p-maximize-current-window)


;; (autoload 'zig-mode "zig-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

(if (>= emacs-major-version 28)
    (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (progn
    (defun colorize-compilation-buffer ()
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point))))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))



;; Dictionary
(setopt ;; dictionary-search-interface   'help  ;; THIS does not work for some reason
        ;; dictionary-default-strategy   "prefix"
        ;; dictionary-default-dictionary "gcide"
        dictionary-server             "dict.org")


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
 '(org-agenda-files
   '("/Users/irubachev/org/20250112T125748--tasks__project.org"))
 '(safe-local-variable-directories
   '("/Users/irubachev/junk/test-gptel/" "/Users/irubachev/repos/random-coffee-bot/" "/Users/irubachev/repos/gptel-test/"
     "/Users/irubachev/repos/velo-pytorch/" "/Users/irubachev/repos/tabular-dl-tabred/"
     "/Users/irubachev/repos/pretrain/" "/Users/irubachev/repos/")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#ffffff")))
 '(header-line ((t :box (:line-width 4 :color "#f2f2f2" :style nil))))
 '(header-line-highlight ((t :box (:color "#000000"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#ffffff")))
 '(mode-line ((t :background "#ffffff" :overline "#000000" :box (:line-width 6 :color "#ffffff" :style nil))))
 '(mode-line-active ((t :background "#ffffff" :overline "#000000" :box (:line-width 6 :color "#ffffff" :style nil))))
 '(mode-line-highlight ((t :box (:color "#000000"))))
 '(mode-line-inactive ((t :background "#ffffff" :overline "#9f9f9f" :box (:line-width 6 :color "#ffffff" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#ffffff" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#c2c2c2" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "#ffffff" :foreground "#ffffff")))
 '(window-divider ((t (:background "#ffffff" :foreground "#ffffff"))))
 '(window-divider-first-pixel ((t (:background "#ffffff" :foreground "#ffffff"))))
 '(window-divider-last-pixel ((t (:background "#ffffff" :foreground "#ffffff")))))
