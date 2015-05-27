;;; plug/latex.el --- latex utility features
;;; plugins related to LaTeX
;;; Commentary:
;;; Code:
(require 'use-package)

(defun keymaps/latex ()
  (evil/set-key evil-insert-state-local-map
                "RET" 'reindent-then-newline-and-indent
                "C-e" 'LaTeX-environment)
  )

(use-package tex-site
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-view-program-selection
        '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
        '(("PDF Viewer" "zathura %o")))
  (add-hook 'LaTeX-mode-hook 'keymaps/latex)
  )

(provide 'plug/latex)
;;; latex.el ends here
