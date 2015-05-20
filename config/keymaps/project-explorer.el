;;; keymaps/project-explorer.el --- Evil mode keymaps for project-explorer
;;; Commentary:
;;; Code:
(defun keymaps/project-explorer ()
  "Keymaps for 'org-mode' in evil mode."
  (evil-set-initial-state 'project-explorer-mode 'normal)
  (evil/set-key evil-normal-state-local-map
                "TAB" 'pe/tab
                "RET" 'pe/return
                "+" 'pe/create-file
                "-" 'pe/delete-file
                "[" 'pe/backward-element
                "]" 'pe/forward-element
                "c" 'pe/copy-file
                "C" 'pe/create-file
                "d" 'pe/delete-file
                "f" 'pe/find-file
                "n" 'next-line
                "p" 'previous-line
                "b" 'previous-line
                "q" 'pe/quit
                "r" 'pe/rename-file
                "s" 'pe/change-directory
                "w" 'pe/copy-file-name-as-kill))

(defun keymaps/speedbar ()
  (evil-set-initial-state 'speedbar-mode 'normal)
  (evil-define-key 'normal speedbar-mode-map
    (kbd "TAB") 'speedbar-toggle-line-expansion
    (kbd ";q") 'sr-speedbar-close))

(advice-add 'sr-speedbar-close :after #'(lambda () (guide-key-mode 1)))
(advice-add 'sr-speedbar-open :after #'(lambda () (guide-key-mode -1)))

(add-hook 'project-explorer-mode-hook 'keymaps/project-explorer)
(add-hook 'speedbar-mode-hook 'keymaps/speedbar)
(provide 'keymaps/project-explorer)
;;; project-explorer.el ends here
