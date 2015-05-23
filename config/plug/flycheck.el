;;; plug/flycheck.el --- Syntax checking
;;; Commentary:
;;; Code:
(require 'use-package)
(use-package flycheck
  :ensure t
  :demand flycheck
  :init
  (global-flycheck-mode t)
  :config
  ;(setq flycheck-highlighting-mode 'lines)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'python-mode-hook
            (lambda ()
              (flycheck-select-checker 'python-pylint)))
  (add-hook 'sh-mode-hook #'(lambda () (flycheck-select-checker 'sh-shellcheck)))
  (add-to-list 'flycheck-shellcheck-supported-shells 'zsh))

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


;; Shellcheck zsh fix
(flycheck-define-checker sh-shellcheck
  "A shell script syntax and style checker using Shellcheck.

See URL `https://github.com/koalaman/shellcheck/'."
  :command ("shellcheck"
            "--format" "checkstyle"
            "--shell" (eval (if (eq sh-shell 'zsh)
                                (symbol-name 'bash)
                              (symbol-name sh-shell)))
            (option "--exclude" flycheck-shellcheck-excluded-warnings list
                    flycheck-option-comma-separated-list)
            source)
  :error-parser flycheck-parse-checkstyle
  :error-filter flycheck-dequalify-error-ids
  :modes sh-mode
  :predicate (lambda () (memq sh-shell flycheck-shellcheck-supported-shells)))

(flycheck-define-checker java-checkstyle
  "A java syntax and style checker using checkstyle."
  :command ("java" "-jar" "/home/dan/.apps/checkstyle-6.6/checkstyle-6.6-all.jar"
            "-c" "/home/dan/.apps/checkstyle-6.6/google_checks.xml"
            "-f" "xml"
            source)
  :error-parser flycheck-parse-checkstyle
  :error-filter (lambda (errors)
                  (flycheck-remove-error-ids
                   (flycheck-sanitize-errors errors)))
  :modes java-mode)
(add-to-list 'flycheck-checkers 'java-checkstyle)

(provide 'plug/flycheck)
;;; flycheck.el ends here
