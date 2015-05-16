;;; plug/my-ido.el --- ido mode?
;;; Commentary:
;;; Code:
(use-package ido
  :demand evil
  :init (ido-mode t)
  :config
  (add-hook 'ido-minibuffer-setup-hook
            'keymaps/ido))
(provide 'plug/my-ido)
;;; plug/my-ido.el ends here
