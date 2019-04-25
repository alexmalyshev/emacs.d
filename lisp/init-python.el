;;; init-python.el --- Configuration for Python.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun init-python--set-blacken-shortcut ()
  "Set keyboard shortcut for running blacken-buffer."
  (local-set-key (kbd "C-c b") 'blacken-buffer))

(defun init-python--hook ()
  "Stuff to run when entering `python-mode'."
  (init-python--set-blacken-shortcut))

(add-hook 'python-mode-hook 'init-python--hook)

(provide 'init-python)
;;; init-python.el ends here
