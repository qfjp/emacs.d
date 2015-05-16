;;; plug/bufcycle.el --- Buffer cycling
;;; Commentary:
;;; Code:
(use-package swbuff-x
  :ensure t
  :demand swbuff-x
  :config
  (setq swbuff-exclude-buffer-regexps '("^ " "^\*.*\*")))

(provide 'plug/bufcycle)
;;; plug/bufcycle.el ends here
