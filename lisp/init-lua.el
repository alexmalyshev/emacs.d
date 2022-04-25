;;; init-lua.el --- Configuration for Lua.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun init-lua--hook ()
  "Stuff to run when entering `lua-mode'."
  (add-to-list 'whitespace-global-modes 'lua-mode))

(if (try-require 'lua-mode)
    (add-hook 'lua-mode-hook 'init-lua--hook))

(provide 'init-lua)
;;; init-lua.el ends here
