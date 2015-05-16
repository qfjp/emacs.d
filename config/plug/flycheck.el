;;; plug/flycheck.el --- Syntax checking
;;; Commentary:
;;; Code:
(use-package flycheck
  :ensure t
  :demand flycheck
  :init
  (global-flycheck-mode t)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'python-mode-hook
            (lambda ()
              (flycheck-select-checker 'python-pylint))))
(provide 'plug/flycheck)
;;; plug/flycheck.el ends here
