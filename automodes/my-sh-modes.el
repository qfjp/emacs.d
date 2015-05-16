;;; package --- summary
;;; Commentary:
;;; Code:
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\zshrc\\'" . sh-mode))

;; Zsh syntax highlighting
(add-hook 'sh-mode-hook
          (lambda ()
            (if (string-match "\\.zsh$" buffer-file-name)
                (sh-set-shell "zsh"))))

(provide 'my-sh-modes)
;;; my-sh-modes.el ends here
