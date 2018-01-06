;; Shortcuts for clang formatting.
(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "C-c r")
          'clang-format-region)))
(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "C-c b")
          'clang-format-buffer)))

(provide 'init-c++)
