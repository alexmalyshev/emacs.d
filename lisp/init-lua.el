;;; init-lua.el --- Configuration for Lua.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lua-mode
  :config
  (setq lua-indent-level 2)
  (add-to-list 'whitespace-global-modes 'lua-mode)
  (eglot-ensure))

(provide 'init-lua)
;;; init-lua.el ends here
