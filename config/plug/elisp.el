;;; plug/elisp.el --- Better documentation search for elisp
;;; Commentary:
;;; Code:
(require 'use-package)
(use-package elisp-slime-nav
  :ensure t
  :demand elisp-slime-nav
  :demand evil
  :config
  (progn
    (defun my-lisp-hook ()
      (eldoc-mode))
    (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)))
(provide 'plug/elisp)
;;; elisp.el ends here
