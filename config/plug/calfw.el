;;; plug/calfw.el --- calfw settings
;;; Commentary:
;;; Code:
(use-package calfw
  :ensure t
  :ensure calfw-gcal
  :config
  (require 'calfw-ical)
  (require 'calfw-gcal)
  (require 'secure/calfw nil 'noerror)
  (add-hook 'cfw:calendar-mode-hook 'keymaps/calfw)
  (add-hook 'cfw:calendar-mode-hook #'(lambda () (visual-line-mode -1))))
(provide 'plug/calfw)
;;; calfw.el ends here
