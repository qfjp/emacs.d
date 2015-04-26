;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package linum-relative
  :ensure t
  :demand linum-relative
  :config
  (add-hook 'after-change-major-mode-hook 'linum-mode))
(provide 'my-linum)
;;; my-linum ends here
