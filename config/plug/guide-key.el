;;; plug/guide-key.el --- Settings for guide-key
;;; Commentary:
;;; Code:

(use-package guide-key
  :ensure t
  :config
  (setq guide-key/guide-key-sequence t)
  (setq guide-key/recursive-key-sequence-flag)
  (setq guide-key/idle-delay 0.4)
  (guide-key-mode t))

(provide 'plug/guide-key)
;;; guide-key.el ends here
