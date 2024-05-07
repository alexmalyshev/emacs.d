;;; init-haskell.el --- Configuration for Haskell.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :config
  ;; Haskell mode makes us enable indentation manually.
  (add-hook 'haskell-mode-hook 'haskell-indentation-hook))

(provide 'init-haskell)
;;; init-haskell.el ends here
