;;; keymaps/ido.el --- Evil keymaps for ido
;;; Commentary:
;;; Code:
(defun keymaps/ido ()
  "Keymaps for ido in evil mode."
  (progn
    (evil/set-key ido-completion-map
                  "C-j" 'ido-next-match
                  "C-n" 'ido-next-match

                  "C-k" 'ido-prev-match
                  "C-b" 'ido-prev-match)))
(provide 'keymaps/ido)
;;; keymaps/ido.el ends here
