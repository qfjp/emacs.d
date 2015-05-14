;;; package --- Summary
;;; Commentary:
;;; Code:
;; Markdown mode
(use-package markdown-mode
  :ensure t
  :init
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
(provide 'my-markdown-modes)
;;; my-markdown-modes.el ends here
