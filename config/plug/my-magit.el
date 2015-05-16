;;; plug/my-magit.el --- Git utility functions
;;; Commentary:
;;; Code:
;; Magit
(use-package magit
  :ensure t
  :ensure magit-gitflow
  :demand magit
  :demand magit-gitflow
  :demand evil
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  ;;(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
  )

(provide 'plug/my-magit)
;;; plug/my-magit ends here
