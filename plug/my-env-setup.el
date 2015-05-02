;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package keychain-environment
  :ensure t
  :demand keychain-environment
  :init
  (keychain-refresh-environment))

(use-package exec-path-from-shell
  :ensure t
  :demand exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))
(provide 'my-env-setup)
;;; my-env-setup ends here
