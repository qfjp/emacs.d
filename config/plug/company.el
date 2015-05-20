;;; plug/company.el --- Autocompletion
;;; Commentary:
;;; Code:
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))
(use-package company
  :ensure t
  :ensure emacs-eclim
  :init
  (progn
    (require 'company-emacs-eclim)
    (company-emacs-eclim-setup)
    (global-company-mode))
  ;;(add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-b") 'company-select-previous)

  ;;(setq company-backends (remove 'company-clang company-backends))
  ;;(push '(company-clang :with company-yasnippet) company-backends)
  )
(provide 'plug/company)
;;; plug/company.el ends here
