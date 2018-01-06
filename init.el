;; -*- emacs-lisp -*-

(defun try-require (name)
  "Try to require a package.  Returns t on success and nil on failure"
  (not (null (require name (symbol-name name) t))))

;; Set up identity correctly.
(setq user-full-name "Alex Malyshev")
(setq user-mail-address "lex.malyshev@gmail.com")

;; Add MELPA packages.
(require 'package)
(let ((melpa '("melpa" . "http://melpa.milkbox.net/packages/")))
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
(setq show-paren-delay 0)
(show-paren-mode t)

;; Emacs gets confused as to what color some terminals are.  Manually set the
;; graphical emacs to light, because it has a white screen, but all the
;; terminals I use have black backgrounds so set them to dark.
(defun better-frame-background (frame)
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

;; Default to 2 space indent and replace tabs with spaces.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 80)

;; Language specific indent levels.
(setq js-indent-level 2)
(setq lua-indent-level 2)
(setq rust-indent-offset 2)
(add-hook 'php-mode-hook (lambda () (setq c-basic-offset 2)))
(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; Haskell mode makes us enable indentation manually.
(add-hook 'haskell-mode-hook 'haskell-indentation-hook)

;; Kill all trailing whitespace upon saving files.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use C-w for killing words like bash does, and move kill-region to C-q.
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-q") 'kill-region)

;; Replace list-buffers with ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Treat .h files as C++ files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Highlight lines past 80 columns in all files, as well as trailing whitespace.
(require 'whitespace)
(setq whitespace-style '(face lines-tail trailing))
(global-whitespace-mode t)

;; Print line comment from current column to fill-column.
(try-require 's)
(defun print-delim ()
  (interactive)
  (let ((size (- fill-column (current-column)))
        (str (s-repeat fill-column (s-trim comment-start))))
    (insert (substring str 0 size))))
(global-set-key (kbd "C-c C-_") 'print-delim)

;; Shortcut for sorting lines.
(global-set-key (kbd "C-c s") 'sort-lines)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(require 'init-c++)
