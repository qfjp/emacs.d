;;; keymaps/help-mode.el --- Evil keybindings for help mode
;;; Commentary:
;;; For some reason, ; -> : keymap needs to be set up like this
;;; Code:

(defun keymaps/help-mode ()
  "Keymaps for `help-mode' in evil mode."
  (evil-set-initial-state 'help-mode 'normal)
  (evil/set-key evil-normal-state-local-map
                ";" 'evil-ex))
(add-hook 'help-mode-hook 'keymaps/help-mode)

(provide 'keymaps/help-mode)
;;; help-mode.el ends here
