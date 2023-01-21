;;; init-haskell.el --- Configuration for Haskell.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Haskell mode makes us enable indentation manually.
(add-hook 'haskell-mode-hook 'haskell-indentation-hook)

(provide 'init-haskell)
;;; init-haskell.el ends here
