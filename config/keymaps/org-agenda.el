;;; keymaps/org-agenda.el --- Evil mode keymaps for org-agenda
;;; Commentary:
;;; Code:
(defun keymaps/org-agenda ()
  "Keymaps for 'org-agenda' in evil mode."
  (eval-after-load 'org-agenda
    '(progn
       (evil-set-initial-state 'org-agenda-mode 'normal))))
(provide 'keymaps/org-agenda)
;;; org-agenda.el ends here
