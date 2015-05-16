;;; plug/my-swbuff-x.el --- Buffer cycling
;;; Commentary:
;;; Code:
(use-package swbuff-x
  :ensure t
  :demand swbuff-x
  :config
  (setq swbuff-exclude-buffer-regexps '("^ " "^\*.*\*")))

(provide 'plug/my-swbuff-x)
;;; plug/my-swbuff-x.el ends here
