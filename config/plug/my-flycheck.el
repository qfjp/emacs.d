;;; plug/my-flycheck.el --- Syntax checking
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
              (flycheck-select-checker 'python-pylint)))
  )
(provide 'plug/my-flycheck)
;;; plug/my-flycheck.el ends here
