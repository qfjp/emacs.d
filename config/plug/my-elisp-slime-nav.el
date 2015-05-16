;;; plug/my-elisp-slime-nav.el --- Better documentation search for elisp
;;; Commentary:
;;; Code:
(use-package elisp-slime-nav
  :ensure t
  :demand elisp-slime-nav
  :demand evil
  :config
  (progn
    (defun my-lisp-hook ()
      (eldoc-mode))
    (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook))
  ;;(evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
  ;;  'elisp-slime-nav-describe-elisp-thing-at-point))
  )
(provide 'plug/my-elisp-slime-nav)
;;; plug/my-elisp-slime-nav.el ends here
