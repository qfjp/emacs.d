;;; package --- Summary
;;; Commentary:
;;; Code:
(defun my-package-list-evil-keymaps ()
  "Keymaps for package in evil mode."
  (eval-after-load 'package
    '(progn
       (evil-set-initial-state 'package-menu-mode 'normal)
       (evil-define-key 'normal package-menu-mode-map
         (kbd "K") 'package-menu-describe-package
         (kbd "u") 'package-menu-mark-delete
         (kbd "i") 'package-menu-mark-install
         (kbd "x") 'package-menu-execute
         (kbd "j") 'evil-next-line
         (kbd "k") 'evil-previous-line))))
(provide 'my-package-list-keys)
;;; my-package-list-keys ends here
