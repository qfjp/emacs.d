;;; plug/ido.el --- ido mode?
;;; Commentary:
;;; Code:
(require 'use-package)
(use-package ido
  :ensure t
  :ensure flx-ido
  :ensure ido-vertical-mode
  :ensure ido-ubiquitous
  :demand evil
  :init
  (progn
    (ido-mode t)
    (ido-vertical-mode t)
    (ido-everywhere t)
    (flx-ido-mode t)
    (ido-ubiquitous-mode t)
    ;; disable ido faces to see flx highlights
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil))
  ;; To disable flx highlights use this:
  ;; (setq flx-ido-use-faces nil)
  :config
  (setq ido-ignore-buffers '("\\` " "^*")))

(use-package smex
  :ensure t
  :init
  (smex-initialize)
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; old M-x
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(provide 'plug/ido)
;;; ido.el ends here
