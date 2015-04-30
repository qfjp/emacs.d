;;; package --- Summary
;;; Commentary:
;;; Code:
(defvar-local sb/visible-lines 0
  "The number (count) of visible lines.")
(defvar-local sb/cur-line 0
  "The current line number.")
(defvar-local sb/lines-from-cur-to-visible-end 0
  "The number of lines from sb/cur-line to the visible end.")
(defvar-local sb/total-lines 0
  "The number of lines in the buffer.")
(defvar-local sb/bot-line-no 0
  "The highest visible line number.")
(defvar-local sb/top-line-no 0
  "The lowest visible line number.")
(defvar-local sb/perc-above-visible 0
  "The percentage of the file above the visible portion.")
(defvar-local sb/perc-below-visible 0
  "The percentage of the file below the visible portion.")
(defvar-local sb/perc-visible 0
  "The percentage of the file that is visible.")
(defvar-local sb/char-count-pre 0
  "The number of characters before the scroll handle.")
(defvar-local sb/char-count-vis 0
  "The number of characters for the scroll handle.")
(defvar-local sb/char-count-pos 0
  "The number of characters after the scroll handle.")
(defvar-local sb/scrollbar-msg ""
  "The string representing the scrollbar")

(defun sb/get-bar-text (size bar handle)
  "Draw a scroll bar of length SIZE using the characters BAR and HANDLE."
  (cond ((minibufferp (current-buffer)) "")
        ((string-match "^\*.*\*" (buffer-name (current-buffer))) "")
         (t
          (setq sb/visible-lines
                (count-lines (window-start) (window-end)))
          (setq sb/cur-line
                (count-lines 1 (point)))
          (setq sb/lines-from-cur-to-visible-end
                (count-lines (point) (window-end)))
          (setq sb/total-lines
                (count-lines 1 (buffer-size)))
          (setq sb/bot-line-no
                (- (+ sb/cur-line sb/lines-from-cur-to-visible-end) 1))
          (setq sb/top-line-no
                (+ (- sb/bot-line-no sb/visible-lines) 1))
          (setq sb/perc-above-visible
                (/ (- sb/top-line-no 1) (float sb/total-lines)))
          (setq sb/perc-below-visible
                (/ (- sb/total-lines (+ 1 sb/bot-line-no)) (float sb/total-lines)))
          (setq sb/perc-visible
                (- 1 (+ sb/perc-above-visible sb/perc-below-visible)))
          (setq sb/char-count-pre
                (floor (* sb/perc-above-visible size)))
          (setq sb/char-count-vis
                (ceiling (* sb/perc-visible size)))
          (setq sb/char-count-pos
                (floor (* sb/perc-below-visible size)))
          (cond
           ((< sb/char-count-pre 0) (setq sb/char-count-pre 0))
           ((< sb/char-count-vis 0) (setq sb/char-count-vis 0))
           ((< sb/char-count-pos 0) (setq sb/char-count-pos 0)))

          (setq sb/scrollbar-msg (concat
                               (make-string sb/char-count-pre bar)
                               (make-string sb/char-count-vis handle)
                               (make-string sb/char-count-pos bar)))
          (sb/fix-width-scrollbar size handle bar)
          
          )
         )
  )

(defun sb/fix-width-scrollbar (size handle bar)
  "Check the width of the scrollbar against SIZE; HANDLE/BAR drawing chars.
This will append/remove chars as necessary."
  (cond
   ((equalp size (string-width sb/scrollbar-msg))
    sb/scrollbar-msg)
   ((< size (string-width sb/scrollbar-msg))
    (substring sb/scrollbar-msg 0 -1))
   ((> size (string-width sb/scrollbar-msg))
    (concat (char-to-string bar) sb/scrollbar-msg)
    )
   )
  )

(provide 'text-scroll-bar)
;;; text-scroll-bar.el ends here
