;;; keymaps/magit --- Evil keymaps for magit
;;; Commentary:
;;; Code:
(defun keymaps/magit ()
  "Evil keymaps for magit"
  (eval-after-load 'magit
    '(progn
      (evil-set-initial-state 'magit-mode 'normal)
      (evil-set-initial-state 'magit-status-mode 'normal)
      (evil-set-initial-state 'magit-diff-mode 'normal)
      (evil-set-initial-state 'magit-log-mode 'normal)
      (evil-set-initial-state 'magit-process-mode 'normal)
      (evil-define-key 'normal magit-mode-map
        (kbd "-") 'magit-diff-smaller-hunks
        (kbd "+") 'magit-diff-larger-hunks
        (kbd "j") 'magit-goto-next-section
        (kbd "k") 'magit-goto-previous-section
        (kbd "c") 'magit-commit
        (kbd "s") 'magit-stage-item
        (kbd "u") 'magit-unstage-item
        (kbd "p") 'magit-toggle-section
        (kbd "P") 'magit-push)
      (evil-define-key 'normal magit-log-mode-map
        (kbd "j") 'magit-goto-next-section
        (kbd "k") 'magit-goto-previous-section)
      (evil-define-key 'normal magit-diff-mode-map
        (kbd "j") 'magit-goto-next-section
        (kbd "k") 'magit-goto-previous-section)
      (evil-define-key 'normal magit-process-mode-map
        (kbd "j") 'evil-next-line
        (kbd "k") 'evil-previous-line))))
(provide 'keymaps/magit)
;;; keymaps/magit.el ends here
