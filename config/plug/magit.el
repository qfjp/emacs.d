;;; plug/magit.el --- Git utility functions
;;; Commentary:
;;; Code:
(require 'use-package)

(defun keymaps/magit ()
  "Evil keymaps for magit"
  (eval-after-load 'magit
    '(progn
       (evil-set-initial-state 'magit-mode 'normal)
       (evil-set-initial-state 'magit-status-mode 'normal)
       (evil-set-initial-state 'magit-diff-mode 'normal)
       (evil-set-initial-state 'magit-log-mode 'normal)
       (evil-set-initial-state 'magit-process-mode 'normal)
       (evil-define-key 'normal magit-mode-map
         (kbd "-") 'magit-diff-smaller-hunks
         (kbd "+") 'magit-diff-larger-hunks
         (kbd "j") 'magit-goto-next-section
         (kbd "k") 'magit-goto-previous-section
         (kbd "c") 'magit-commit
         (kbd "s") 'magit-stage-item
         (kbd "u") 'magit-unstage-item
         (kbd "p") 'magit-toggle-section
         (kbd "P") 'magit-key-mode-popup-pushing
         (kbd "F") 'magit-key-mode-popup-pulling
         (kbd "r") 'magit-refresh
         (kbd "q") 'kill-buffer-and-window)

       (evil-define-key 'normal magit-log-mode-map
         (kbd "j") 'magit-goto-next-section
         (kbd "k") 'magit-goto-previous-section)

       (evil-define-key 'normal magit-diff-mode-map
         (kbd "j") 'magit-goto-next-section
         (kbd "k") 'magit-goto-previous-section)

       (evil-define-key 'normal magit-process-mode-map
         (kbd "j") 'evil-next-line
         (kbd "k") 'evil-previous-line))))

(use-package magit
  :ensure t
  :ensure magit-gitflow
  :demand magit
  :demand magit-gitflow
  :demand evil
  :demand ido
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-completing-read-function 'magit-ido-completing-read)
  ;;(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
  )

(add-hook 'magit-mode-hook 'keymaps/magit)

(provide 'plug/magit)
;;; magit.el ends here
