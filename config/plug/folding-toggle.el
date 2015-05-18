;;; plug/folding-toggle.el --- Folding ala vim
;;; The main function 'my-setup-folding-by-marks' automatically
;;; switches the default folding keys (in evil mode) to use folding.el
;;; Commentary:
;;; Code:

(defun my-setup-folding-by-marks ()
  "Setup folding based on markers."
  (load "plug/folding" 'nomessage 'noerror)
  (folding-mode-add-find-file-hook)
  (folding-add-to-marks-list 'sh-mode "# {{{" "# }}}")

  (defun my-folding-fold-keymaps ()
    (if folded-file
        (evil/set-key evil-normal-state-local-map
                      "z[" 'folding-open-buffer
                      "z]" 'folding-whole-buffer
                      "zo" 'folding-show-current-entry
                      "zc" 'folding-hide-current-entry)))

  (defun my-org-fold-keymaps ()
    (progn
      (evil/set-key evil-normal-state-local-map
                    "z[" 'show-all
                    "z]" 'org-overview)))
  (add-hook 'folding-mode-hook 'my-folding-fold-keymaps)

  (add-hook 'org-mode-hook 'my-org-fold-keymaps)
  (add-hook 'prog-mode-hook 'hs-minor-mode))
(provide 'plug/folding-toggle)

;;; plug/folding-toggle.el ends here
