(setq c-basic-offset 2)

;; Don't indent on opening a C++ namespace, an extern block, or on case labels.
(c-set-offset 'inextern-lang 0)
(c-set-offset 'innamespace 0)
(c-set-offset 'case-label 0)
(c-set-offset 'arglist-intro '+)
(c-set-offset 'arglist-close 0)

;; Shortcuts for clang formatting.
(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "C-c r")
          'clang-format-region)))
(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "C-c b")
          'clang-format-buffer)))

(provide 'init-c++)
