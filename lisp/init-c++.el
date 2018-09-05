;;; init-c++.el --- Configuration for c++-mode.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst c-basic-offset 2)

(defun set-c++-offsets ()
  "Set C++ indentation offsets to my rules.
Don't indent on opening a C++ namespace, an extern block, or on case labels."
  (c-set-offset 'inextern-lang 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'case-label 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0))

(defconst flycheck-clang-language-standard "c++17")

(defun set-clang-format-shortcuts ()
  "Set keyboard shortcuts for running clang-format."
  (local-set-key (kbd "C-c r") 'clang-format-region)
  (local-set-key (kbd "C-c b") 'clang-format-buffer))

(let ((hook (lambda ()
              "Stuff to run when entering `c++-mode'."
              (set-clang-format-shortcuts)
              (flycheck-mode -1))))
  (add-hook 'c++-mode-hook hook))

(provide 'init-c++)
;;; init-c++.el ends here
