;;; plug/helm.el --- Better M-x menu
;;; Commentary:
;;; Code:
(require 'use-package)
(use-package helm
  :ensure t
  :demand helm
  :config
  (global-set-key (kbd "M-x") 'helm-M-x))

(provide 'plug/helm)
;;; helm.el ends here
