;;; package --- Summary
;;; Commentary:
;;; Code:
(defun my-ibuffer-evil-keymaps ()
  "Keymaps for ibuffer in evil mode."
  (eval-after-load 'ibuffer
    '(progn
       (evil-set-initial-state 'ibuffer-mode 'normal)
       (evil-define-key 'normal ibuffer-mode-map
         (kbd "J") 'ibuffer-jump-to-buffer
         (kbd "j") 'evil-next-line
         (kbd "k") 'evil-previous-line
         (kbd "l") 'ibuffer-visit-buffer
         (kbd "v") 'ibuffer-toggle-marks))))
(provide 'my-ibuffer-keys)
;;; my-ibuffer-keys ends here
