;;; plug/irony.el --- Irony mode (C/C++)
;;; Commentary:
;;; Code:
(use-package irony
  :ensure t
  :ensure company-irony
  :init
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode))
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(provide 'plug/irony)
;;; irony.el ends here
