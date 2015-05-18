;;; keymaps/org-agenda.el --- Evil mode keymaps for org-agenda
;;; Commentary:
;;; Code:
(defun keymaps/org-agenda ()
  "Keymaps for 'org-agenda' in evil mode."
  (eval-after-load 'org-agenda
    '(progn
       (evil-set-initial-state 'org-agenda-mode 'normal))))

(defun keymaps/org ()
  "Keymaps for 'org-mode' in evil mode."
  (evil/set-key evil-normal-state-local-map
                "RET" 'org-open-at-point))

(add-hook 'org-mode-hook 'keymaps/org)

(provide 'keymaps/org-agenda)
;;; org-agenda.el ends here
