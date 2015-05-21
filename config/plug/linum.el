;;; plug/linum.el --- Relative line numbering
;;; Commentary:
;;; Code:
(require 'use-package)
(use-package linum-relative
  :ensure t
  :demand linum-relative
  :config
  (setq linum-relative-current-symbol "")
  (setq linum-relative-format "%3s ")
  (add-hook 'after-change-major-mode-hook 'linum-mode))
(provide 'plug/linum)
;;; linum.el ends here
