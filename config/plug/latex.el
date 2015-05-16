;;; plug/latex.el --- latex utility features
;;; plugins related to LaTeX
;;; Commentary:
;;; Code:

(use-package tex-site
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-view-program-selection
        '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
        '(("PDF Viewer" "zathura %o"))))

(provide 'plug/latex)
;;; plug/latex.el ends here
