;;; plug/env-setup.el --- Seems necessary for magit push
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
(provide 'plug/env-setup)
;;; plug/env-setup ends here
