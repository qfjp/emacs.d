;;; plug/paredit.el --- package configuration for paredit
;;; Commentary:
;;; Code:
(use-package paredit
  :ensure t
  :ensure paredit-everywhere
  :ensure evil-paredit
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook       'evil-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook 'evil-paredit-mode)
    (add-hook 'ielm-mode-hook             'evil-paredit-mode)
    (add-hook 'lisp-mode-hook             'evil-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'evil-paredit-mode)
    (add-hook 'scheme-mode-hook           'evil-paredit-mode))
  :config
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

(provide 'plug/paredit)
;;; plug/paredit.el ends here
