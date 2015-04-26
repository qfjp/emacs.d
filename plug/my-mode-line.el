;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package powerline-evil
  :ensure t
  :demand evil
  :init
  (powerline-evil-vim-color-theme)
  :config
  (display-time-mode t))

(provide 'my-mode-line)
;;; my-mode-line ends here
