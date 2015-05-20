;;; keymaps/package-list.el --- Evil keymaps for package list
;;; Commentary:
;;; Code:
(defun keymaps/package-list ()
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

(add-hook 'package-menu-mode-hook 'keymaps/package-list)
(provide 'keymaps/package-list)
;;; keymaps/package-list.el ends here
