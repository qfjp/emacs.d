;;; plug/ido.el --- ido mode?
;;; Commentary:
;;; Code:
(use-package ido
  :demand evil
  :init (ido-mode t)
  :config
  (add-hook 'ido-minibuffer-setup-hook
            'keymaps/ido))
(provide 'plug/ido)
;;; plug/ido.el ends here
