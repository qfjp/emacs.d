;;; plug/project-search.el --- Utilities for searching a project
;;; Commentary:
;;; Code:
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode))

;; (use-package project-explorer
;;   :ensure t)
(add-to-list 'load-path (concat user-emacs-directory "config/plug/project-explorer"))
(eval-after-load 'projectile
  '(progn
     (setq pe/cache-enabled)
     (require 'project-explorer)
     (add-hook 'project-explorer-mode-hook '(lambda () (visual-line-mode -1)))))

(provide 'plug/project-search)
;;; project-search.el ends here
