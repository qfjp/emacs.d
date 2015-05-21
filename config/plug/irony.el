;;; plug/irony.el --- Irony mode (C/C++)
;;; Commentary:
;;; Code:
(require 'use-package)
(use-package irony
  :ensure t
  :ensure company-irony
  :ensure flycheck-irony
  :ensure irony-eldoc
  :init
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode))
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

  ;; flycheck
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

  ;; eldoc
  (eval-after-load 'irony
    '(add-hook 'irony-mode-hook #'irony-eldoc)))

(provide 'plug/irony)
;;; irony.el ends here
