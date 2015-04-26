;;; package --- Summary
;;; Commentary:
;;; Code:
;; Magit
(use-package magit
  :ensure t
  :demand magit
  :demand evil
  :config
  (progn
    (setq magit-auto-revert-mode nil)
    (setq magit-last-seen-setup-instructions "1.4.0")))

(provide 'my-magit)
;;; my-magit ends here
