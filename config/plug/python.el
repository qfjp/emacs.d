;;; plug/python.el --- Python mode settings
;;; Commentary:
;;; Code:
(use-package anaconda-mode
  :ensure t
  :ensure company-anaconda
  :demand company
  :config
  (add-to-list 'company-backends 'company-anaconda)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))

(defun anaconda-mode-doc-buffer (doc)
  "Display documentation buffer with contents DOC."
  (let ((buf (get-buffer-create "*anaconda-doc*")))
    (with-current-buffer buf
      (help-mode)
      (view-mode -1)
      (erase-buffer)
      (insert doc)
      (goto-char (point-min))
      (view-mode 1)
      buf)))

(provide 'plug/python)
;;; python.el ends here
