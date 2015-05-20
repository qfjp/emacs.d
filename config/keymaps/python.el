;;; keymaps/python.el --- Keymaps for python mode
;;; Commentary:
;;; Code:
(defun keymaps/python ()
  "Keymaps for python mode."
  (evil/set-key evil-normal-state-local-map
                "K" 'anaconda-mode-view-doc))

(add-hook 'python-mode-hook 'keymaps/python)

(provide 'keymaps/python)
;;; python.el ends here
