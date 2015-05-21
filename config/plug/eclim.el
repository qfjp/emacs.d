;;; plug/eclim.el --- Settings for eclim mode
;;; Commentary:
;;; Code:
(require 'use-package)
(add-hook 'eclim-project-mode-hook 'keymaps/eclim)
(use-package emacs-eclim
  :ensure t
  :demand company
  :init
  (progn
    (require 'eclim)
    (require 'eclimd)
    (global-eclim-mode)
    (add-hook 'java-mode-hook
              #'(lambda () (setq-local company-minimum-prefix-length 1)))))

(provide 'plug/eclim)
;;; eclim.el ends here
