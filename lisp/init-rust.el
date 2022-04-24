;;; init-rust.el --- Configuration for Rust.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar rust-indent-offset 4)

(defun init-rust--set-format-shortcut ()
  "Set rust-format-buffer keyboard shortcut."
  (local-set-key (kbd "C-c b") 'rust-format-buffer))

(defun init-rust--hook ()
  "Stuff to run when entering `rust-mode'."
  (init-rust--set-format-shortcut)
  (flycheck-mode -1))

(add-hook 'rust-mode-hook 'init-rust--hook)

(provide 'init-rust)
;;; init-rust.el ends here
