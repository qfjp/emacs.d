;;; package --- Summary
;;; Commentary:
;;; Code:
(defun my-ido-evil-keymaps ()
  "Keymaps for ido in evil mode."
  (progn
    (define-key ido-completion-map (kbd "l") 'ido-next-match)
    (define-key ido-completion-map (kbd "h") 'ido-prev-match)))
(provide 'my-ido-keys)
;;; my-ido-keys ends here
