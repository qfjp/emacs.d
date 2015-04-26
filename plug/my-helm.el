;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package helm
  :ensure t
  :demand helm
  :config
  (global-set-key (kbd "M-x") 'helm-M-x))

(provide 'my-helm)
;;; my-helm ends here
