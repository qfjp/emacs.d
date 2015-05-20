;;; keymaps/ibuffer.el --- Evil keymaps for ibuffer
;;; Commentary:
;;; Code:
(defun keymaps/ibuffer ()
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
(add-hook 'ibuffer-mode-hook 'keymaps/ibuffer)
(provide 'keymaps/ibuffer)
;;; keymaps/ibuffer ends here
