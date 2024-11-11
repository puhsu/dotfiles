;; -*- lexical-binding: t; -*-

;; All the packages are installed with nix, see ../home.nix for the list of packages

;; (setq package-enable-at-startup nil) all packages are installed by nix, but autoloads are handled by package.el

(setq inhibit-default-init nil)
(setq frame-inhibit-implied-resize t)

(add-to-list 'initial-frame-alist '(width . 200))
(add-to-list 'initial-frame-alist '(height . 100))

(add-to-list 'default-frame-alist '(width . 200))
(add-to-list 'default-frame-alist '(height . 100))


;; Following snippets are from https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/early-init.el

(defun p-emacs-re-enable-frame-theme (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`prot-emacs-avoid-initial-flash-of-light'."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

;; (setq mode-line-format nil)       
(set-face-attribute 'default nil :background "#ffffff" :foreground "#000000")
(set-face-attribute 'mode-line nil :background "#ffffff" :foreground "#000000" :box 'unspecified)
(add-hook 'after-make-frame-functions #'p-emacs-re-enable-frame-theme)



