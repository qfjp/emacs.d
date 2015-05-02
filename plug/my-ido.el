;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package ido
  :demand evil
  :init (ido-mode t)
  :config
  (add-hook 'ido-minibuffer-setup-hook
            'my-ido-evil-keymaps))
(provide 'my-ido)
;;; my-ido ends here
