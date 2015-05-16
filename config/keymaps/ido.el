;;; keymaps/ido.el --- Evil keymaps for ido
;;; Commentary:
;;; Code:
(defun keymaps/ido-evil ()
  "Keymaps for ido in evil mode."
  (progn
    (define-key ido-completion-map (kbd "l") 'ido-next-match)
    (define-key ido-completion-map (kbd "h") 'ido-prev-match)))
(provide 'keymaps/ido)
;;; keymaps/ido.el ends here
