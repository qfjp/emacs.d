;;; package --- Summary
;;; Commentary:
;; To get the total number of lines, you can use 'count-lines-page'
;;; Code:
(require 'evil)
(require 'git-modeline)
(use-package nyan-mode
  :ensure t)

;; Colors
(tty-color-define "green" 151)
(tty-color-define "blue" 67)
(tty-color-define "purple" 139)
(tty-color-define "orange" 173)
(tty-color-define "red" 131)
(tty-color-define "pink" 182)

;; State vars
(defvar ml/normal-color)
(defvar ml/insert-color)
(defvar ml/visual-color)
(defvar ml/replace-color)
(defvar ml/cur-state-bg)
(defvar ml/state-fg)
(defvar ml/main-fg)
(defvar ml/main-bg)
(setq ml/main-bg "#2f2f2f")

;; colors
(defvar ml/secondary-normal-bg)
(setq ml/secondary-normal-bg "#454545")
(defvar ml/secondary-dark-bg)
(setq ml/secondary-dark-bg "black")
(defvar ml/secondary-fg)
(setq ml/secondary-fg "black")

(defvar ml/secondary-bg)
(setq ml/secondary-bg ml/secondary-normal-bg)

(defun ml/refresh-colors ()
  "Set colors based on whether we are in a GUI or tty."
  (setq ml/state-fg "gray12")
  (setq ml/normal-color "blue")
  (setq ml/insert-color "green")
  (setq ml/visual-color "orange")
  (setq ml/replace-color "red")

  (setq ml/cur-state-bg "blue")
  (setq ml/main-fg "gray96")
  (if window-system
      (progn
        (setq ml/normal-color "#6a95b5")
        (setq ml/insert-color "#99cc99")
        (setq ml/visual-color "#d28445")
        (setq ml/replace-color "#ac4142")
        (setq ml/cur-state-bg ml/normal-color)
        )))

(ml/refresh-colors)

;; Mode line info vars
(defvar ml/evil-state-msg)
(defvar ml/secondary-state-msg)
(defvar ml/buffer-name-msg)
(defvar major-mode-msg nil
  "The major mode string to display.")
(defvar ml/encoding-msg nil
  "The text to use for the buffer encoding.")
(defvar ml/ruler-msg nil
  "The text to use for the mode-line ruler.")
(defvar ml/left-right-buffer nil
  "Size of the gap between the left and right.")

(defun ml/create-left-section (text fg bg nextbg &optional face-args)
  "Create a section with TEXT with colors FG/BG.
This will be a section on the left of the status bar."
  (concat
   (propertize
    (concat " " text " ")
    'face
    (append `(:foreground ,fg :background ,bg) face-args))
   (propertize (char-to-string ?\ue0b0)
               'face `(:foreground ,bg :background ,nextbg))))

(defun ml/create-right-section (text fg bg prevbg &optional face-args)
  "Create a section with TEXT with colors FG/BG.
This will be a section on the left of the status bar."
  (concat
   (propertize (char-to-string ?\ue0b2)
               'face `(:foreground ,bg :background ,prevbg))
   (propertize
    (concat " " text " ")
    'face
    (append `(:foreground ,fg :background ,bg) face-args))))

;; Refresh state
(defun ml/refresh-evil-state-msg ()
  "Return the state of evil mode."
  (cond
   ((evil-normal-state-p)
    (setq ml/secondary-bg ml/secondary-normal-bg)
    (setq ml/cur-state-bg ml/normal-color)
    (setq ml/secondary-fg ml/main-fg)
    (setq ml/evil-state-msg
          (ml/create-left-section
           "NORMAL" ml/state-fg ml/normal-color ml/secondary-bg
           '(:weight bold))))
   ((evil-insert-state-p)
    (setq ml/secondary-bg ml/secondary-dark-bg)
    (setq ml/cur-state-bg ml/insert-color)
    (setq ml/secondary-fg ml/cur-state-bg)
    (setq ml/evil-state-msg
          (ml/create-left-section
           "INSERT" ml/state-fg ml/insert-color ml/secondary-bg
           '(:weight bold))))
   ((evil-visual-state-p)
    (setq ml/secondary-bg ml/secondary-dark-bg)
    (setq ml/cur-state-bg ml/visual-color)
    (setq ml/secondary-fg ml/cur-state-bg)
    (setq ml/evil-state-msg
          (ml/create-left-section
           "VISUAL" ml/state-fg ml/visual-color ml/secondary-bg
           '(:weight bold))))
   ((evil-replace-state-p)
    (setq ml/secondary-bg ml/secondary-normal-bg)
    (setq ml/cur-state-bg ml/replace-color)
    (setq ml/evil-state-msg
          (ml/create-left-section
           "REPLACE" ml/state-fg ml/replace-color ml/secondary-bg
           '(:weight bold)))))
  )

(defun ml/refresh-major-mode-msg ()
  "Give an indicator for the major mode."
  (setq
   major-mode-msg
   (ml/create-right-section
     mode-name ml/main-fg ml/main-bg ml/main-bg)))

(defun ml/refresh-buffer-name-msg ()
  "Give an indicator for the current file."
  (setq ml/buffer-name-msg
        (concat
         (propertize " "
                     'face
                     `(:background ,ml/main-bg))
         (propertize " %b " 'face '(:background "black")))))

(defun ml/refresh-encoding-msg ()
  "Return the encoding section."
  (setq ml/encoding-msg
        (ml/create-right-section
         (symbol-name buffer-file-coding-system)
         ml/secondary-fg
         ml/secondary-bg
         ml/main-bg)))

(defun ml/refresh-ruler-msg ()
  "Set the value of the ruler message."
  (setq-default nyan-bar-length 10)
  (setq-default nyan-cat-face-number 1)
  (setq-default nyan-wavy-trail t)
  (setq ml/ruler-msg
        (concat
         " " (nyan-create) "  %3l: %3c "))
  (setq ml/ruler-msg (ml/create-right-section
                   ml/ruler-msg
                   ml/state-fg
                   ml/cur-state-bg
                   ml/secondary-bg)))

(defun ml/refresh-secondary-state-msg ()
  "Return an secondary second state."
  (setq ml/secondary-state-msg
        (ml/create-left-section (get-git-branch)
                                     ml/secondary-fg
                                     ml/secondary-bg
                                     ml/main-bg)))

(defadvice ml/evil-refresh-mode-line
    (after ml/refresh-evil-state-msg activate)
  "Update the indicator for the modeline state."
  (ml/refresh-evil-state-msg)
  (ml/refresh-secondary-state-msg)
  (ml/refresh-buffer-name-msg)
  (ml/refresh-major-mode-msg)
  (ml/refresh-encoding-msg)
  (ml/refresh-ruler-msg))

(defvar ml/mode-line-left "" "The left mode line message.")
(defvar ml/mode-line-right "" "The right mode line message.")

(defvar ml/truncate-depth nil "Whether the mode-line has to be truncated.")

(defvar-local ml/fudge-val -4)

(defun ml/calculate-buffer-width ()
  "Calculate the number of spaces between left and right."
  (let ((fudge-val ml/fudge-val))
    (when (display-graphic-p)
      (setq fudge-val (- fudge-val 5)))
    (setq num-spaces (- (window-width)
                        (+ (string-width (format-mode-line ml/mode-line-left))
                           (string-width (format-mode-line ml/mode-line-right)))
                        fudge-val ;; quick hack to fill out buffer in console
                        )))
  num-spaces)

(defun ml/refresh-left-msg ()
  "Set the left portion of the mode-line."
  (setq-default
   ml/mode-line-left
   (list
    '(:eval ml/evil-state-msg)
    '(:eval ml/secondary-state-msg)
    '(:eval ml/buffer-name-msg))))

(defun ml/refresh-right-msg ()
  "Set the right portion of the mode-line."
  (setq-default
   ml/mode-line-right
   (list
    '(:eval major-mode-msg)
    '(:eval ml/encoding-msg)
    '(:eval ml/ruler-msg))))

(defun write-spaces (length accum)
  "Writes LENGTH number of spaces to the accumulator ACCUM."
  (cond
   ((equalp length 0) accum)
   (t (write-spaces (- length 1) (concat accum " ")))))


(defun ml/refresh-left-right-buffer ()
  "Set the size of the gap between the left and right messages."
  (setq ml/left-right-buffer
        (write-spaces
         (ml/calculate-buffer-width)
         "")))

(defun ml/refresh-mode-line (&optional arg arg2 arg3)
  "Refresh the whole mode line; ARG, ARG2 and ARG3 provided to satisfy scroll hooks."
  (ml/refresh-colors)
  (ml/refresh-evil-state-msg)
  (ml/refresh-secondary-state-msg)
  (ml/refresh-buffer-name-msg)
  (ml/refresh-major-mode-msg)
  (ml/refresh-encoding-msg)
  (ml/refresh-ruler-msg)

  (ml/refresh-left-msg)
  (ml/refresh-right-msg)
  (ml/refresh-left-right-buffer))

(setq-default
 mode-line-format
 (list
  '(:eval ml/mode-line-left)
  '(:eval (propertize ml/left-right-buffer 'face `(:background ,ml/main-bg)))
  '(:eval ml/mode-line-right)))

(advice-add 'evil-next-line :after
            #'ml/refresh-mode-line)
(advice-add 'evil-next-visual-line :after
            #'ml/refresh-mode-line)
(advice-add 'evil-previous-line :after
            #'ml/refresh-mode-line)
(advice-add 'evil-previous-visual-line :after
            #'ml/refresh-mode-line)
(advice-add 'evil-forward-char :after
            #'ml/refresh-mode-line)
(advice-add 'evil-backward-char :after
            #'ml/refresh-mode-line)

(add-hook 'window-scroll-functions #'ml/refresh-mode-line)
;(add-hook 'after-change-major-mode-hook #'refresh-mode-line)

(provide 'my-mode-line)
;;; my-mode-line.el ends here
