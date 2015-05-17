;;; plug/ido.el --- ido mode?
;;; Commentary:
;;; Code:
(use-package ido
  :ensure t
  :ensure flx-ido
  :demand evil
  :init
  (progn
    (ido-mode t)
    (ido-everywhere t)
    (flx-ido-mode t)
    ;; disable ido faces to see flx highlights
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil))
  ;; To disable flx highlights use this:
  ;; (setq flx-ido-use-faces nil)
  :config
  (add-hook 'ido-minibuffer-setup-hook
            'keymaps/ido))
(provide 'plug/ido)
;;; plug/ido.el ends here
