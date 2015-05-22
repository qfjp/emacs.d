;;; keymaps/flycheck.el --- evil maps for flycheck list errors
;;; Commentary:
;;; Code:
(defun keymaps/flycheck ()
  "Evil maps for flycheck error list."
  (evil/set-key evil-normal-state-local-map
                "?" 'describe-mode
                "j" 'flycheck-error-list-next-error
                "k" 'flycheck-error-list-previous-error
                "RET" 'flycheck-error-list-goto-error))
(add-hook 'flycheck-error-list-mode-hook 'keymaps/flycheck)
(provide 'keymaps/flycheck)
;;; flycheck.el ends here
