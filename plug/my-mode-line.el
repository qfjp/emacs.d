;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'evil)

;; State vars
(defvar normal-color)
(defvar insert-color)
(defvar visual-color)
(defvar replace-color)
(defvar cur-state-color)
(setq-default normal-color "#4a708b")
(setq-default insert-color "#9acd32")
(setq-default visual-color "#ffdead")
(setq-default replace-color "#cd5c5c")
(setq-default cur-state-color "#cd5c5c")

(defvar normal-face)
(defvar insert-face)
(defvar visual-face)
(defvar replace-face)


;; Mode line info vars
(defvar evil-state-msg)
(defvar optional-state-msg)
(defvar buffer-name-msg)
(defvar major-mode-msg)
(defvar encoding-msg)
(defvar ruler-msg)

;; Indicate the state of the last separator
(defvar sep-indicator)
(setq-default sep-indicator "orig")


;; colors
(defvar optional-section-back-color)
(setq optional-section-back-color "#454545")
(defvar main-section-back-color)
(setq main-section-back-color "#2f2f2f")
(defvar encoding-section-back-color)
(setq encoding-section-back-color "#454545")

(setq-default
 normal-face
 `(:foreground "gray12" :background ,normal-color :height 0.9 :weight bold))
(setq-default
 insert-face
 `(:foreground "gray12" :background ,insert-color :height 0.9 :weight bold))
(setq-default
 visual-face
 `(:foreground "gray12" :background ,visual-color :height 0.9 :weight bold))
(setq-default
 replace-face
 `(:foreground "gray12" :background ,replace-color :height 0.9 :weight bold))

(defvar new-fore)
(defvar new-back)

;; Main hook
(defun refresh-evil-state-msg ()
  "Return the state of evil mode."
  (setq new-back optional-section-back-color)
  (cond
   ((evil-normal-state-p)
    (setq cur-state-color normal-color)
    (setq evil-state-msg
          (concat (propertize " NORMAL " 'face normal-face)
                  (make-right-separator normal-face))))
   ((evil-insert-state-p)
    (setq cur-state-color insert-color)
    (setq evil-state-msg
          (concat (propertize " INSERT " 'face insert-face)
                  (make-right-separator insert-face))))
   ((evil-visual-state-p)
    (setq cur-state-color visual-color)
    (setq evil-state-msg
          (concat (propertize " VISUAL " 'face visual-face)
                  (make-right-separator visual-face))))
   ((evil-replace-state-p)
    (setq cur-state-color replace-color)
    (setq evil-state-msg
          (concat (propertize " REPLACE " 'face replace-face)
                  (make-right-separator replace-face)))))
  (setq sep-indicator "state")
  (refresh-optional-state-msg)
  (setq sep-indicator "optional")
  (refresh-buffer-name-msg)
  (setq sep-indicator "mode")
  (refresh-major-mode-msg)
  (setq sep-indicator "encoding")
  (refresh-encoding-msg)
  (refresh-ruler-msg)
  )

(defun refresh-major-mode-msg ()
  "Give an indicator for the major mode."
  (setq
   major-mode-msg
   (concat
    (propertize (message "  %s " major-mode)
                'face `(:background ,main-section-back-color))
    (make-left-separator `(:background ,main-section-back-color)))))

(defun refresh-buffer-name-msg ()
  "Give an indicator for the current file."
  (setq buffer-name-msg
        (concat
         (propertize " "
                     'face
                     `(:background ,main-section-back-color))
         (propertize " %b " 'face '(:background "black")))))

(defun make-right-separator (face)
  "Given a FACE plist, make the appropriate separator."
  (setq new-fore (plist-get face :background))
  (cond
   ((equalp sep-indicator "state")
    (setq new-back main-section-back-color))
   ((equalp sep-indicator "optional")
    (setq new-back optional-section-back-color)))
  (propertize "" 'face `(:foreground ,new-fore :background ,new-back)))

(defun make-left-separator (face)
  "Given a FACE plist, make the appropriate separator."
  (cond
   ((equalp sep-indicator "mode")
    (setq new-fore encoding-section-back-color))
   ((equalp sep-indicator "encoding")
    (setq new-fore cur-state-color)
    )
   )
  (setq new-back (plist-get face :background))
  (propertize "" 'face `(:foreground ,new-fore :background ,new-back)))

(defun refresh-encoding-msg ()
  "Return the encoding section."
  (setq encoding-msg
        (concat
         (propertize
          (message " %s " buffer-file-coding-system)
          'face
          `(:background ,encoding-section-back-color))
         (make-left-separator
          `(:background ,encoding-section-back-color)))))

(defun refresh-ruler-msg ()
  "Set the value of the ruler message."
  (setq ruler-msg
        (propertize "  %l: %c " 'face `(:background ,cur-state-color))))

(defun refresh-optional-state-msg ()
  "Return an optional second state."
  (setq optional-state-msg
        (make-right-separator
         `(:foreground "gray19"
                       :background ,optional-section-back-color
                       :height 0.9
                       :weight bold))))

(defadvice evil-refresh-mode-line (after refresh-evil-state-msg activate)
    "Update the indicator for the modeline state."
    (refresh-evil-state-msg))

(setq-default
 mode-line-format
 (list
  '(:eval evil-state-msg)
  '(:eval optional-state-msg) ; for git status, etc
  '(:eval buffer-name-msg)
  '(:eval major-mode-msg)
  '(:eval encoding-msg)
  '(:eval ruler-msg)
  ;;'(:eval (propertize (concat " [" mode-name "] %l:%i\t") 'face `(:foreground "black" :background ,main-section-back-color :weight normal)
  ;;                    'help-echo (buffer-file-name)))
  ))
(provide 'my-mode-line)
;;; my-mode-line ends here
