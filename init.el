;;; init.el --- Init script.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun try-require (name)
  "Try to require a package called NAME.  Return t/nil success status."
  (not (null (require name (symbol-name name) t))))

;; Set up identity correctly.
(setq user-full-name "Alex Malyshev")
(setq user-mail-address "lex.malyshev@gmail.com")

;; Add MELPA packages.
(require 'package)
(let ((melpa '("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives melpa t))
(package-initialize)

;; Default buffer is an empty file in org-mode.
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; Save backup and autosave files to the temp directory.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Show row,column numbers.
(line-number-mode t)
(column-number-mode t)

;; Turn off all menu bars that I never use.
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp `tool-bar-mode)
  (tool-bar-mode -1))

;; Speed up echoing.  Can't be 0 as that means to turn it off.
(setq echo-keystrokes 0.1)

;; Always show matching parens/braces/brackets, and speed them up too.
(defvar show-paren-delay 0)
(show-paren-mode t)

(defun better-frame-background (frame)
  "Emacs gets confused as to what color some terminals are.
Manually set the graphical Emacs to light, because it has a white
screen, but all the terminals I use have black backgrounds so set
them to dark.  FRAME is passed to 'frame-set-background-mode'."
  (let ((frame-background-mode (if (display-graphic-p) 'light 'dark)))
    (frame-set-background-mode frame)))

(if (daemonp)
    (add-to-list 'after-make-frame-functions 'better-frame-background t)
  (better-frame-background nil))

;; Enable the mouse.
(xterm-mouse-mode t)

;; Disable all version control.
(setq vc-handled-backends nil)

;; Automatically revert buffers as they're modified.
(global-auto-revert-mode 1)

;; Always syntax highlight.
(global-font-lock-mode 1)

;; Enable sub-words.
(global-subword-mode 1)

(when (fboundp 'global-flycheck-mode)
  (global-flycheck-mode))

;; Default to 2 space indent and replace tabs with spaces.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 80)

;; Language specific indent levels.
(defvar js-indent-level 2)
(defvar lua-indent-level 2)
(add-hook 'php-mode-hook (lambda () (defvar c-basic-offset 2)))
(defvar sh-basic-offset 2)
(defvar sh-indentation 2)
(defvar typescript-indent-level 2)

;; Haskell mode makes us enable indentation manually.
(add-hook 'haskell-mode-hook 'haskell-indentation-hook)

;; Kill all trailing whitespace upon saving files.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use C-w for killing words like bash does, and move kill-region to C-q.
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-q") 'kill-region)

;; Replace list-buffers with ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun better-split-window-below (&optional size)
  "This is a variant of 'split-window-below' that auto-balances windows.
Passes through the SIZE argument."
  (interactive)
  (split-window-below size)
  (balance-windows))

(defun better-split-window-right (&optional size)
  "This is a variant of 'split-window-right' that auto-balances windows.
Passes through the SIZE argument."
  (interactive)
  (split-window-right size)
  (balance-windows))

;; Always rebalance windows after splitting them.
(global-set-key (kbd "C-x 2") 'better-split-window-below)
(global-set-key (kbd "C-x 3") 'better-split-window-right)

;; Treat .h files as C++ files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Highlight lines past 80 columns in all files, as well as trailing whitespace.
(require 'whitespace)
(setq whitespace-style '(face lines-tail trailing))
(global-whitespace-mode t)
(setq whitespace-global-modes ())

;; Shortcut for sorting lines.
(global-set-key (kbd "C-c s") 'sort-lines)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(defvar flycheck-emacs-lisp-load-path 'inherit)

(require 'init-c++)
(require 'init-python)
(require 'init-rust)

(provide 'init)
;;; init.el ends here
