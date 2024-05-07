;;; init-lua.el --- Configuration for Lua.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar lua-indent-level 2)

(defun init-lua--hook ()
  "Stuff to run when entering `lua-mode'."
  (add-to-list 'whitespace-global-modes 'lua-mode))

(use-package lsp-mode)

(use-package lua-mode
  :config
  (add-hook 'lua-mode-hook 'init-lua--hook)
  (add-hook 'lua-mode-hook 'lsp))

(provide 'init-lua)
;;; init-lua.el ends here
