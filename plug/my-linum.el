;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package linum-relative
  :ensure t
  :demand linum-relative
  :config
  (setq linum-relative-current-symbol "")
  (setq linum-relative-format "%3s ")
  (add-hook 'after-change-major-mode-hook 'linum-mode))
(provide 'my-linum)
;;; my-linum ends here
