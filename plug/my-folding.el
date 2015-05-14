;;; package --- Summary
;;; The main function 'my-setup-folding-by-marks' automatically
;;; switches the default folding keys (in evil mode) to use folding.el
;;; Commentary:
;;; Code:

(defun my-setup-folding-by-marks ()
  "Setup folding based on markers."
  (load "folding" 'nomessage 'noerror)
  (folding-mode-add-find-file-hook)
  (folding-add-to-marks-list 'sh-mode "# {{{" "# }}}")

  (defun my-change-fold-keymaps ()
    (if folded-file
        (progn
          (define-key evil-normal-state-local-map (kbd "z[")
            'folding-open-buffer)
          (define-key evil-normal-state-local-map (kbd "z]")
            'folding-whole-buffer)
          (define-key evil-normal-state-local-map (kbd "zo")
            'folding-show-current-entry)
          (define-key evil-normal-state-local-map (kbd "zc")
            'folding-hide-current-entry))))
  (defadvice folding-mode (after folding-mode activate)
    (my-change-fold-keymaps))
  (ad-activate 'folding-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))
(provide 'my-folding)

;;; my-folding.el ends here
