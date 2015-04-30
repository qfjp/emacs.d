;;; package --- Summary
;;; Commentary:
;; To get the total number of lines, you can use 'count-lines-page'
;;; Code:
(require 'evil)
(require 'git-modeline)
;; State vars
(defvar normal-color)
(defvar insert-color)
(defvar visual-color)
(defvar replace-color)
(defvar cur-state-color)
(defvar text-color)
(setq-default text-color "gray12")
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
 `(:foreground
   ,text-color
   :background
   ,normal-color
   :height
   0.9
   :weight bold))
(setq-default
 insert-face
 `(:foreground
   ,text-color
   :background
   ,insert-color
   :height
   0.9
   :weight
   bold))
(setq-default
 visual-face
 `(:foreground
   ,text-color
   :background
   ,visual-color
   :height
   0.9
   :weight
   bold))
(setq-default
 replace-face
 `(:foreground
   ,text-color
   :background
   ,replace-color
   :height
   0.9
   :weight
   bold))

(defvar new-fore)
(defvar new-back)
(require 'text-scroll-bar)

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
  (refresh-scrollbar-msg)
  (refresh-ruler-msg)
  (refresh-left-right-buffer))

(defun refresh-scrollbar-msg ()
  (setq scrollbar-msg
        (sb/get-bar-text 10 ?\u25A0 ?\u25AB)))


(defun refresh-major-mode-msg ()
  "Give an indicator for the major mode."
  (setq
   major-mode-msg
   (concat
    (propertize (concat "  " mode-name " ")
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
  (propertize (char-to-string ?\ue0b0)
              'face `(:foreground ,new-fore :background ,new-back)))

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
  (propertize (char-to-string ?\ue0b2)
              'face `(:foreground ,new-fore :background ,new-back)))

(defun refresh-encoding-msg ()
  "Return the encoding section."
  (setq encoding-msg
        (concat
         (propertize
          (concat " " (symbol-name buffer-file-coding-system) " ")
          'face
          `(:background ,encoding-section-back-color))
         (make-left-separator
          `(:background ,encoding-section-back-color)))))

(defun refresh-ruler-msg ()
  "Set the value of the ruler message."
  (setq ruler-msg
        (concat
         (propertize scrollbar-msg 'face `(:background
                                           ,cur-state-color
                                           :foreground
                                           ,text-color))
         (propertize "  "
                     'face `(:background
                             ,cur-state-color
                             :foreground
                             ,text-color))
         (propertize "%3l:" 'face `(:background
                                    ,cur-state-color
                                    :foreground
                                    ,text-color
                                    :weight
                                    bold))
         (propertize "%3c " 'face `(:background
                                    ,cur-state-color
                                    :foreground
                                    ,text-color)))))

(defun refresh-optional-state-msg ()
  "Return an optional second state." 
  (setq optional-state-msg
        (concat
         (propertize (get-git-branch) 'face
                     `(:background ,optional-section-back-color
                       :height 0.9))
         (make-right-separator
          `(:foreground "gray19"
                        :background ,optional-section-back-color
                        :height 0.9
                        :weight bold)))))

(defadvice evil-refresh-mode-line (after refresh-evil-state-msg activate)
    "Update the indicator for the modeline state."
    (refresh-evil-state-msg))

(defvar mode-line-left "" "The left mode line message.")
(defvar mode-line-right "" "The right mode line message.")


(defun refresh-left-msg ()
  "Set the left portion of the mode-line."
  (setq-default
   mode-line-left
   (list
    '(:eval evil-state-msg)
    '(:eval optional-state-msg)
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
         (- (window-width)
            (+ (string-width (format-mode-line mode-line-left))
               (string-width (format-mode-line mode-line-right)))
            -4
            )
         "")))

(defun refresh-mode-line (&optional arg arg2 arg3)
  "Refresh the whole mode line; ARG, ARG2 and ARG3 provided to satisfy scroll hooks."
  (refresh-evil-state-msg)
  (refresh-left-msg)
  (refresh-right-msg)
  (refresh-left-right-buffer))

(setq-default
 mode-line-format
 (list
  '(:eval mode-line-left)
  '(:eval (propertize left-right-buffer 'face `(:background ,main-section-back-color)))
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
