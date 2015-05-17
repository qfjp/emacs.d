;;; plug/magit.el --- Git utility functions
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :ensure magit-gitflow
  :demand magit
  :demand magit-gitflow
  :demand evil
  :demand ido
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-completing-read-function 'magit-ido-completing-read)
  ;;(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
  )

(provide 'plug/magit)
;;; plug/magit ends here
