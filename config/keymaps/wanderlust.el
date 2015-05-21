;;; keymaps/wanderlust.el --- keymaps for evil wanderlust
;;; Commentary:
;;; Code:
(defun keymaps/wanderlust-folder ()
  "Keymaps for evil wanderlust folder view."
  (evil/set-key evil-normal-state-local-map
                "TAB" 'wl-folder-revisit-last-visited-folder
                "RET" 'wl-folder-jump-to-current-entity
                "E" 'wl-folder-empty-trash
                "F" 'wl-folder-flush-queue
                "[" 'wl-folder-open-all
                "]" 'wl-folder-close-all
                "f" 'wl-folder-goto-first-unread-folder
                "j" 'wl-folder-next-entity
                "k" 'wl-folder-prev-entity
                "J" 'wl-folder-next-unread
                "K" 'wl-folder-prev-unread
                "q" 'wl-exit
                ))
(defun keymaps/wanderlust-summary ()
  "Keymaps for evil wanderlust mail view."
  (evil/set-key evil-normal-state-local-map
                "RET" 'wl-summary-enter-handler
                "TAB" 'wl-summary-goto-last-displayed-msg
                "u" 'wl-summary-mark-as-unread
                "i" 'wl-summary-mark-as-important
                "j" 'wl-summary-next
                "k" 'wl-summary-prev
                "q" 'wl-summary-exit
                "s" 'wl-summary-sync
                "w" 'wl-summary-write))

(add-hook 'wl-folder-mode-hook 'keymaps/wanderlust-folder)
(add-hook 'wl-summary-mode-hook 'keymaps/wanderlust-summary)
(provide 'keymaps/wanderlust)
;;; wanderlust.el ends here
