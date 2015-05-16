;;; plug/my-helm.el --- Better M-x menu
;;; Commentary:
;;; Code:

(use-package helm
  :ensure t
  :demand helm
  :config
  (global-set-key (kbd "M-x") 'helm-M-x))

(provide 'plug/my-helm)
;;; plug/my-helm.el ends here
