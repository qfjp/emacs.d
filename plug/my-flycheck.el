;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package flycheck
  :ensure t
  :demand flycheck
  :init
  (global-flycheck-mode t)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
(provide 'my-flycheck)
;;; my-flycheck ends here
