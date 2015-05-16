;;; keymaps/dired.el --- Evil keymaps for dired
;;; Commentary:
;;; Code:
(defun keymaps/dired ()
  "Evil keymaps for dired-x."
  (require 'dired-x)
  (put 'dired-find-alternate-file 'disabled nil)
  (eval-after-load 'dired
    '(progn
       (evil-set-initial-state 'dired-mode 'normal)
       (defun my-dired-up-directory ()
         "Take dired up one directory, but behave like
                         dired-find-alternate-file."
         (interactive)
         (let ((old (current-buffer)))
           (dired-up-directory)
           (kill-buffer old)))
       (evil-define-key 'normal dired-mode-map "h"
         'my-dired-up-directory)
       (evil-define-key 'normal dired-mode-map "l"
         'dired-find-alternate-file)
       (evil-define-key 'normal dired-mode-map "o"
         'dired-sort-toggle-or-edit)
       (evil-define-key 'normal dired-mode-map "v"
         'dired-toggle-marks)
       (evil-define-key 'normal dired-mode-map "m" 'dired-mark)
       (evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
       (evil-define-key 'normal dired-mode-map "U"
         'dired-unmark-all-marks)
       (evil-define-key 'normal dired-mode-map "c"
         'dired-create-directory)
       (evil-define-key 'normal dired-mode-map "n"
         'evil-search-next)
       (evil-define-key 'normal dired-mode-map "N"
         'evil-search-previous)
       (evil-define-key 'normal dired-mode-map ";" 'evil-ex)
       (evil-define-key 'normal dired-mode-map "q"
         'kill-this-buffer))))

(provide 'keymaps/dired)
;;; keymaps/dired.el ends here
