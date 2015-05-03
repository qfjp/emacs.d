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
(defvar normal-color)
(defvar insert-color)
(defvar visual-color)
(defvar replace-color)
(defvar cur-state-bg)
(defvar state-fg)
(defvar main-fg)
(defvar main-bg)
(setq main-bg "#2f2f2f")

;; colors
(defvar secondary-normal-bg)
(setq secondary-normal-bg "#454545")
(defvar secondary-dark-bg)
(setq secondary-dark-bg "black")
(defvar secondary-fg)
(setq secondary-fg "black")


(defvar secondary-bg)
(setq secondary-bg secondary-normal-bg)

(defvar secondary-face)

(defun refresh-colors ()
  "Set colors based on whether we are in a GUI or tty."
  (setq state-fg "gray12")
  (setq normal-color "blue")
  (setq insert-color "green")
  (setq visual-color "orange")
  (setq replace-color "red")

  (setq cur-state-bg "blue")
  (setq main-fg "gray96")
  (if window-system
      (progn
        (setq normal-color "#6a95b5")
        (setq insert-color "#99cc99")
        (setq visual-color "#d28445")
        (setq replace-color "#ac4142")
        (setq cur-state-bg normal-color)
        )))

(refresh-colors)

;; Mode line info vars
(defvar evil-state-msg)
(defvar secondary-state-msg)
(defvar buffer-name-msg)
(defvar major-mode-msg nil
  "The major mode string to display.")
(defvar encoding-msg nil
  "The text to use for the buffer encoding.")
(defvar ruler-msg nil
  "The text to use for the mode-line ruler.")
(defvar left-right-buffer nil
  "Size of the gap between the left and right.")
(defvar scrollbar-msg ""
  "The scrollbar text widget.")

(defun airline/create-left-section (text fg bg nextbg &optional face-args)
  "Create a section with TEXT with colors FG/BG.
This will be a section on the left of the status bar."
  (concat
   (propertize
    (concat " " text " ")
    'face
    (append `(:foreground ,fg :background ,bg) face-args))
   (propertize (char-to-string ?\ue0b0)
               'face `(:foreground ,bg :background ,nextbg))))

(defun airline/create-right-section (text fg bg prevbg &optional face-args)
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
(defun refresh-evil-state-msg ()
  "Return the state of evil mode."
  (cond
   ((evil-normal-state-p)
    (setq secondary-bg secondary-normal-bg)
    (setq cur-state-bg normal-color)
    (setq secondary-fg main-fg)
    (setq evil-state-msg
          (airline/create-left-section
           "NORMAL" state-fg normal-color secondary-bg
           '(:weight bold))))
   ((evil-insert-state-p)
    (setq secondary-bg secondary-dark-bg)
    (setq cur-state-bg insert-color)
    (setq secondary-fg cur-state-bg)
    (setq evil-state-msg
          (airline/create-left-section
           "INSERT" state-fg insert-color secondary-bg
           '(:weight bold))))
   ((evil-visual-state-p)
    (setq secondary-bg secondary-dark-bg)
    (setq cur-state-bg visual-color)
    (setq secondary-fg cur-state-bg)
    (setq evil-state-msg
          (airline/create-left-section
           "VISUAL" state-fg visual-color secondary-bg
           '(:weight bold))))
   ((evil-replace-state-p)
    (setq secondary-bg secondary-normal-bg)
    (setq cur-state-bg replace-color)
    (setq evil-state-msg
          (airline/create-left-section
           "REPLACE" state-fg replace-color secondary-bg
           '(:weight bold)))))
  )

(defun refresh-major-mode-msg ()
  "Give an indicator for the major mode."
  (setq
   major-mode-msg
   (airline/create-right-section
     mode-name main-fg main-bg main-bg)))

(defun refresh-buffer-name-msg ()
  "Give an indicator for the current file."
  (setq buffer-name-msg
        (concat
         (propertize " "
                     'face
                     `(:background ,main-bg))
         (propertize " %b " 'face '(:background "black")))))

(defun refresh-encoding-msg ()
  "Return the encoding section."
  (setq encoding-msg
        (airline/create-right-section
         (symbol-name buffer-file-coding-system)
         secondary-fg
         secondary-bg
         main-bg)))

(defun refresh-ruler-msg ()
  "Set the value of the ruler message."
  (setq-default nyan-bar-length 10)
  (setq-default nyan-cat-face-number 1)
  (setq-default nyan-wavy-trail t)
  (setq ruler-msg
        (concat
         " " (nyan-create) "  %3l: %3c "))
  (setq ruler-msg (airline/create-right-section
                   ruler-msg
                   state-fg
                   cur-state-bg
                   secondary-bg)))

(defun refresh-secondary-state-msg ()
  "Return an secondary second state."
  (setq secondary-state-msg
        (airline/create-left-section (get-git-branch)
                                     secondary-fg
                                     secondary-bg
                                     main-bg)))

(defadvice evil-refresh-mode-line (after refresh-evil-state-msg activate)
  "Update the indicator for the modeline state."
  (refresh-evil-state-msg)
  (refresh-secondary-state-msg)
  (refresh-buffer-name-msg)
  (refresh-major-mode-msg)
  (refresh-encoding-msg)
  (refresh-ruler-msg))

(defvar mode-line-left "" "The left mode line message.")
(defvar mode-line-right "" "The right mode line message.")

(defvar truncate-depth nil "Whether the mode-line has to be truncated.")

(defvar-local ml/fudge-val -4)

(defun calculate-buffer-width ()
  "Calculate the number of spaces between left and right."
  (when (display-graphic-p)
    (setq ml/fudge-val -16))
  (setq num-spaces (- (window-width)
     (+ (string-width (format-mode-line mode-line-left))
        (string-width (format-mode-line mode-line-right)))
     ml/fudge-val ;; quick hack to fill out buffer in console
     ))
  num-spaces)

(defun refresh-left-msg ()
  "Set the left portion of the mode-line."
  (setq-default
   mode-line-left
   (list
    '(:eval evil-state-msg)
    '(:eval secondary-state-msg)
    '(:eval buffer-name-msg))))

(defun refresh-right-msg ()
  "Set the right portion of the mode-line."
  (setq-default
   mode-line-right
   (list
    '(:eval major-mode-msg)
    '(:eval encoding-msg)
    '(:eval ruler-msg))))

(defun write-spaces (length accum)
  "Writes LENGTH number of spaces to the accumulator ACCUM."
  (cond
   ((equalp length 0) accum)
   (t (write-spaces (- length 1) (concat accum " ")))))


(defun refresh-left-right-buffer ()
  "Set the size of the gap between the left and right messages."
  (setq left-right-buffer
        (write-spaces
         (calculate-buffer-width)
         "")))

(defun refresh-mode-line (&optional arg arg2 arg3)
  "Refresh the whole mode line; ARG, ARG2 and ARG3 provided to satisfy scroll hooks."
  (refresh-colors)
  (refresh-evil-state-msg)
  (refresh-secondary-state-msg)
  (refresh-buffer-name-msg)
  (refresh-major-mode-msg)
  (refresh-encoding-msg)
  (refresh-ruler-msg)

  (refresh-left-msg)
  (refresh-right-msg)
  (refresh-left-right-buffer))

(setq-default
 mode-line-format
 (list
  '(:eval mode-line-left)
  '(:eval (propertize left-right-buffer 'face `(:background ,main-bg)))
  '(:eval mode-line-right)))

(advice-add 'evil-next-line :after
            #'refresh-mode-line)
(advice-add 'evil-previous-line :after
            #'refresh-mode-line)
(advice-add 'evil-forward-char :after
            #'refresh-mode-line)
(advice-add 'evil-backward-char :after
            #'refresh-mode-line)

(add-hook 'window-scroll-functions #'refresh-mode-line)

(provide 'my-mode-line)
;;; my-mode-line.el ends here
