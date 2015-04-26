;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package fill-column-indicator
  :ensure t
  :demand fill-column-indicator
  :config
  (setq fci-rule-character #x2593)
  (setq fci-rule-color "gray19")
  (add-hook 'after-change-major-mode-hook 'fci-mode)
  (setq fci-rule-column 70))

;; Fill column indicator fix for autocomplete
(defun sanityinc/fci-enabled-p ()
  "Some internet fix for auto-complete with fci."
  (symbol-value 'fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend 'fci-mode' and 'linum-mode' while popups are visible."
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (setq sanityinc/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore 'fci-mode' and 'linum mode' when all popups have closed."
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

(provide 'my-fill-column-indicator)
;;; my-fill-column-indicator ends here
