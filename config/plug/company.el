;;; plug/company.el --- Autocompletion
;;; Commentary:
;;; Code:
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))
(use-package company
  :ensure t
  :ensure company-anaconda
  :ensure anaconda-mode
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

  ;; python completion
  (add-to-list 'company-backends 'company-anaconda)
  (add-hook 'python-mode-hook 'anaconda-mode)

  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  ;;(defun company-mode/backend-with-yas (backend)
  ;;  "Add snippet completion to company menus given a BACKEND list."
  ;;  (if (or (not company-mode/enable-yas)
  ;;          (and (listp backend)
  ;;               (member 'company-yasnippet backend)))
  ;;      backend
  ;;    (append (if (consp backend) backend (list backend))
  ;;            '(:with company-yasnippet))))


  ;;(setq company-backends
  ;;      (mapcar #'company-mode/backend-with-yas company-backends)))
  )
(provide 'plug/company)
;;; plug/company.el ends here
