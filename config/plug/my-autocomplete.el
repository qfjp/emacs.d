;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'use-package)
(use-package yasnippet
  :ensure t)

(use-package auto-complete
  :ensure t
  :demand auto-complete
  :demand yasnippet
  :init
  (yas-global-mode t)
  (ac-config-default)
  (require 'auto-complete-config)
  :config
  (ac-linum-workaround)
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>")
  (setq ac-auto-show-menu t)
  (setq ac-show-menu-immediately-on-auto-complete t)
  (setq ac-use-menu-map t)
  (defun add-yasnippet-ac-sources ()
    "Add yasnippet to autocomplete sources."
    (add-to-list 'ac-sources 'ac-source-yasnippet))
  (add-hook 'after-change-major-mode-hook 'add-yasnippet-ac-sources)
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous))

(use-package jedi
  :ensure t
  :demand jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (setq jedi:complete-on-dot t))
(provide 'my-autocomplete)
;;; my-autocomplete ends here
