;;; plug/wanderlust.el --- Wanderlust settings
;;; Commentary:
;;; Code:
(use-package wanderlust
  :ensure t
  :init
  (autoload 'wl "wl" "Wanderlust" t)
  :config
  (setq elmo-imap4-default-server "imap.gmail.com"
        elmo-imap4-default-user "djpade@gmail.com"
        elmo-imap4-default-authenticate-type 'clear
        elmo-imap4-default-port '993
        elmo-imap4-default-stream-type 'ssl
        elmo-imap4-use-modified-utf7 t)
  (setq wl-smtp-connection-type 'starttls
        wl-smtp-posting-port 587
        wl-smtp-authenticate-type "plain"
        ;;wl-smtp-authenticate-type "login"
        wl-smtp-posting-user "djpade"
        wl-smtp-posting-server "smtp.gmail.com"
        wl-local-domain "gmail.com"
        wl-message-id-domain "smtp.gmail.com")
  (setq wl-from "Daniel Pade <djpade@gmail.com>"

        ;;all system folders (draft, trash, spam, etc) are placed in the
        ;;[Gmail]-folder, except inbox. "%" means it's an IMAP-folder
        wl-default-folder "%inbox"
        wl-draft-folder   "%[Gmail]/Drafts"
        wl-trash-folder   "%[Gmail]/Trash"
        wl-fcc            "%[Gmail]/Sent"

        ;; mark sent messages as read (sent messages get sent back to you and
        ;; placed in the folder specified by wl-fcc)
        wl-fcc-force-as-read    t

        ;;for when auto-compleating foldernames
        wl-default-spec "%"))
(add-hook 'mime-view-mode-hook #'(lambda () (setq show-trailing-whitespace nil)))
(provide 'plug/wanderlust)
;;; wanderlust.el ends here
