;;; plug/project-search.el --- Utilities for searching a project
;;; Commentary:
;;; Code:
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode))

(use-package project-explorer
  :ensure t)
(provide 'plug/project-search)
;;; plug/project-search.el ends here
