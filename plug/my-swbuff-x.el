;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package swbuff-x
  :ensure t
  :demand swbuff-x
  :config
  (setq swbuff-exclude-buffer-regexps '("^ " "^\*.*\*")))

(provide 'my-swbuff-x)
;;; my-swbuff-x ends here
