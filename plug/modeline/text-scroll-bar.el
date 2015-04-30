;;; package --- Summary
;;; Commentary:
;;; Code:
(defvar-local visible-lines 0
  "The number (count) of visible lines.")
(defvar-local cur-line 0
  "The current line number.")
(defvar-local lines-from-cur-to-visible-end 0
  "The number of lines from cur-line to the visible end.")
(defvar-local total-lines 0
  "The number of lines in the buffer.")
(defvar-local bot-line-no 0
  "The highest visible line number.")
(defvar-local top-line-no 0
  "The lowest visible line number.")
(defvar-local percentage-above-visible 0
  "The percentage of the file above the visible portion.")
(defvar-local percentage-below-visible 0
  "The percentage of the file below the visible portion.")
(defvar-local percentage-visible 0
  "The percentage of the file that is visible.")
(defvar-local char-count-pre 0
  "The number of characters before the scroll handle.")
(defvar-local char-count-vis 0
  "The number of characters for the scroll handle.")
(defvar-local char-count-pos 0
  "The number of characters after the scroll handle.")
(defvar-local scrollbar-msg ""
  "The string representing the scrollbar")

(defun get-bar-text (size bar handle)
  "Draw a scroll bar of length SIZE using the characters BAR and HANDLE."
  (cond ((minibufferp (current-buffer)) "")
         ((equalp (point) 1) (concat (char-to-string handle) (make-string 9 bar)))
         (t
          (setq visible-lines
                (count-lines (window-start) (window-end)))
          (setq cur-line
                (count-lines 1 (point)))
          (setq lines-from-cur-to-visible-end
                (count-lines (point) (window-end)))
          (setq total-lines
                (count-lines 1 (buffer-size)))
          (setq bot-line-no
                (- (+ cur-line lines-from-cur-to-visible-end) 1))
          (setq top-line-no
                (+ (- bot-line-no visible-lines) 1))
          (setq percentage-above-visible
                (/ (- top-line-no 1) (float total-lines)))
          (setq percentage-below-visible
                (/ (- total-lines (+ 1 bot-line-no)) (float total-lines)))
          (setq percentage-visible
                (- 1 (+ percentage-above-visible percentage-below-visible)))
          (setq char-count-pre
                (floor (* percentage-above-visible size)))
          (setq char-count-vis
                (ceiling (* percentage-visible size)))
          (setq char-count-pos
                (floor (* percentage-below-visible size)))
          (cond
           ((< char-count-pre 0) (setq char-count-pre 0))
           ((< char-count-vis 0) (setq char-count-vis 0))
           ((< char-count-pos 0) (setq char-count-pos 0)))

          (setq scrollbar-msg (concat
                               (make-string char-count-pre bar)
                               (make-string char-count-vis handle)
                               (make-string char-count-pos bar)))

          (cond
           ((equalp (string-width scrollbar-msg) size) scrollbar-msg)
           ((< (string-width scrollbar-msg) size)
            (setq char-count-vis (concat char-count-vis handle))
            (setq scrollbar-msg (concat
                                 (make-string char-count-pre bar)
                                 (make-string char-count-vis handle)
                                 (make-string char-count-pos bar))))
           ((> (string-width scrollbar-msg) size) (substring scrollbar-msg 0 -1))))))

(provide 'text-scroll-bar)
;;; text-scroll-bar.el ends here
