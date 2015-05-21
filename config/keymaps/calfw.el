;;; keymaps/calfw.el --- Evil maps for calfw
;;; Commentary:
;;; Code:
(defun keymaps/calfw ()
  "Keymaps for calfw."
  (evil-set-initial-state 'cfw:calendar-mode 'normal)
  (evil/set-key evil-normal-state-local-map
                "h" 'cfw:navi-previous-day-command
                "j" 'cfw:navi-next-week-command
                "k" 'cfw:navi-previous-week-command
                "l" 'cfw:navi-next-day-command
                "q" 'bury-buffer
                "r" 'cfw:refresh-calendar-buffer
                "t" 'cfw:navi-goto-today-command
                "TAB" 'cfw:show-details-command
                "$" 'cfw:navi-goto-week-end-command
                "0" 'cfw:navi-goto-week-begin-command
                ">" 'cfw:navi-next-month-command
                "<" 'cfw:navi-previous-month-command
                "D" 'cfw:change-view-day
                "M" 'cfw:change-view-month
                "W" 'cfw:change-view-week
                "o" 'cfw:navi-goto-date-command))
(provide 'keymaps/calfw)
;;; calfw.el ends here
