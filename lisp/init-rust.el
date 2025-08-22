;;; init-rust.el --- Configuration for Rust.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :config
  (setq rust-indent-offset 4)
  (local-set-key (kbd "C-c b") 'rust-format-buffer)
  (flycheck-mode -1)
  (eglot-ensure))

(provide 'init-rust)
;;; init-rust.el ends here
