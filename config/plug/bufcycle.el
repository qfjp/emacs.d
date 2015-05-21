;;; plug/bufcycle.el --- Buffer cycling
;;; Commentary:
;;; Code:
(require 'use-package)
(use-package swbuff-x
  :ensure t
  :demand swbuff-x
  :config
  (setq swbuff-exclude-buffer-regexps '("^ " "^\*.*\*")))

(provide 'plug/bufcycle)
;;; bufcycle.el ends here
