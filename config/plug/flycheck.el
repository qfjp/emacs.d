;;; plug/flycheck.el --- Syntax checking
;;; Commentary:
;;; Code:
(use-package flycheck
  :ensure t
  :demand flycheck
  :init
  (global-flycheck-mode t)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'python-mode-hook
            (lambda ()
              (flycheck-select-checker 'python-pylint))))

(defun activate-flyspell ()
  "Force flyspell mode."
  (flyspell-mode t))
(custom-set-variables
 '(ispell-dictionary "en_US")
 '(ispell-program-name "/usr/bin/aspell"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'org-mode-hook 'activate-flyspell)
(add-hook 'text-mode-hook 'activate-flyspell)
(add-hook 'message-mode-hook 'activate-flyspell)

(provide 'plug/flycheck)
;;; flycheck.el ends here
