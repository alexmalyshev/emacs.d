;;; init-rust.el --- Configuration for rust-mode.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar rust-indent-offset 2)

(defun init-rust--set-format-shortcut ()
  "Set rust-format-buffer keyboard shortcut."
  (local-set-key (kbd "C-c b") 'rust-format-buffer))

(defun init-rust--hook ()
  "Stuff to run when entering `rust-mode'."
  (init-rust--set-format-shortcut))

(add-hook 'rust-mode-hook 'init-rust--hook)

(provide 'init-rust)
;;; init-rust.el ends here
