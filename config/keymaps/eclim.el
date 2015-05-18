;;; keymaps/eclim.el --- Evil keymaps for eclim (project manager)
;;; Commentary:
;;; Code:
(defun keymaps/eclim ()
  "Keymaps for ido in evil mode."
  (evil-define-key 'normal eclim-project-mode-map
    (kbd "RET") 'eclim-project-goto
    (kbd "d") 'eclim-project-delete
    (kbd "c") 'eclim-project-create
    (kbd "C") 'eclim-project-close
    (kbd "r") 'eclim-project-mode-refresh
    (kbd "q") 'eclim-quit-window
    (kbd "m") 'eclim-project-mark-current
    (kbd "u") 'eclim-project-unmark-current))

(provide 'keymaps/eclim)
;;; keymaps/ido.el ends here
