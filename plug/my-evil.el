;;; package --- Summary
;;; EVIL EVIL EVIL
;;; Commentary:
;;; Code:
(use-package key-chord
  :ensure t
  :demand key-chord
  :demand evil
  :init
  (key-chord-mode 1)
  :config
  (key-chord-define-global "jk" 'evil-normal-state))

(use-package evil-leader
  :commands (evil-leader-mode)
  :ensure t
  :demand evil-leader
  :init
  (global-evil-leader-mode)
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "w" 'save-buffer)
    (evil-leader/set-key "q" 'kill-buffer-and-window)
    (evil-leader/set-key "b" 'ido-switch-buffer)))

(use-package evil-search-highlight-persist
  :commands (evil-search-highlight-persist)
  :ensure t
  :demand evil-search-highlight-persist
  :demand evil-leader
  :demand evil
  :init
  (global-evil-search-highlight-persist t)
  :config
  (evil-leader/set-key "SPC"
    'evil-search-highlight-persist-remove-all))

(use-package evil-surround
  :ensure t
  :demand evil
  :init
  (global-evil-surround-mode t))

(use-package evil-numbers
  :ensure t
  :demand evil
  :config
  (global-set-key (kbd "C-a") 'evil-numbers/inc-at-pt))

(use-package evil
  :ensure t
  :commands (evil)
  :demand evil
  :demand swbuff-x
  :config
  (progn
    (setq evil-search-wrap nil)
    (evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
      'elisp-slime-nav-describe-elisp-thing-at-point)
    (define-key evil-normal-state-map (kbd "gh") 'help-command)
    (define-key key-translation-map (kbd "gx") (kbd "C-x"))
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-normal-state-map (kbd "C-n")
      'swbuff-switch-to-next-buffer)
    (define-key evil-normal-state-map (kbd "C-b")
      'swbuff-switch-to-previous-buffer)
    (define-key evil-normal-state-map (kbd ";") 'evil-ex)
    (define-key evil-ex-map (kbd "w ;") 'save-buffer) ; quick save

    (define-key evil-normal-state-map (kbd "z[") 'hs-show-all)
    (define-key evil-normal-state-map (kbd "z]") 'hs-hide-all)
    (my-setup-folding-by-marks)
    ;;(add-hook 'evil-insert-state-entry-hook (lambda () (linum-mode -1)))
    ;;(add-hook 'evil-insert-state-exit-hook (lambda () (linum-mode)))
    (my-dired-evil-keymaps)
    (my-package-list-evil-keymaps)
    (my-ibuffer-evil-keymaps)
    (my-magit-evil-keymaps)
    (evil-mode 1)))

(provide 'my-evil)
;;; my-evil.el ends here