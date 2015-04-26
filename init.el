;;; package --- Summary
;;; Commentary:

;;; Code:

;; disable menubar, toolbar, and scrollbars
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; paren matching (highlight expression)
(show-paren-mode 1)
;;(setq show-paren-style 'expression)

;; stop prompting about git symlinks
(setq vc-follow-symlinks t)

;; Minibuffer persistent history
(savehist-mode 1)

;; disable start screen
(setq inhibit-splash-screen t)

;; spaces not tabs
(setq-default indent-tabs-mode nil)

;; disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Emacs evil will use c-u to scroll
(setq evil-want-C-u-scroll t)

;; set the default font
(set-frame-font "Fantasque Sans Mono-10")

;; Keymap configurations
(add-to-list 'load-path (concat user-emacs-directory "keymaps"))

(require 'my-ibuffer-keys)
(require 'my-package-list-keys)
(require 'my-dired-keys)

;; Plugin Configurations
(add-to-list 'load-path (concat user-emacs-directory "plug"))

(require 'my-package)
(require 'my-use-package)
(require 'my-env-setup)
(require 'my-swbuff-x)
(require 'my-helm)
(require 'my-linum)
(require 'my-autocomplete)
(require 'my-flycheck)

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
    (evil-leader/set-key "b" 'ido-switch-buffer)
    ))

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

(use-package powerline-evil
  :ensure t
  :demand evil
  :init
  (powerline-evil-vim-color-theme)
  :config
  (display-time-mode t))

;; Elisp slime nav
(use-package elisp-slime-nav
  :ensure t
  :demand elisp-slime-nav
  :demand evil
  :config
  (progn
    (defun my-lisp-hook ()
      (eldoc-mode))
    (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook))
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
    'elisp-slime-nav-describe-elisp-thing-at-point))

;; Theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("261f5ddd72a1c0b47200e13d872075af5f78c3f07d2968bddc0301261934f210"
     "8022cea21aa4daca569aee5c1b875fbb3f3248a5debc6fc8cf5833f2936fbb22"
     default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background
                         "dim gray" :foreground "#202020")))))
(load-theme 'base16-default-dark)

;; Fill column indicator
(defun sanityinc/fci-enabled-p ()
  "Some internet fix for auto-complete with fci."
  (symbol-value 'fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend 'fci-mode' and 'linum-mode' while popups are visible."
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (setq sanityinc/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore 'fci-mode' and 'linum mode' when all popups have closed."
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

(add-hook 'evil-insert-state-entry-hook (lambda () (linum-mode -1)))
(add-hook 'evil-insert-state-exit-hook (lambda () (linum-mode)))
                    

(use-package fill-column-indicator
  :ensure t
  :demand fill-column-indicator
  :config
  (add-hook 'after-change-major-mode-hook 'fci-mode)
  (setq fci-rule-column 70))

(require 'ansi-color)
(defun display-ansi-colors ()
  "Display colors from ansi codes."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'magit-process-mode-hook 'display-ansi-colors)
;; Magit
(use-package magit
  :ensure t
  :demand magit
  :demand evil
  :config
  (progn
    (setq magit-auto-revert-mode nil)
    (setq magit-last-seen-setup-instructions "1.4.0")
    (evil-set-initial-state 'magit-mode 'normal)
    (evil-set-initial-state 'magit-status-mode 'normal)
    (evil-set-initial-state 'magit-diff-mode 'normal)
    (evil-set-initial-state 'magit-log-mode 'normal)
    (evil-set-initial-state 'magit-process-mode 'normal)
    (evil-define-key 'normal magit-mode-map
      "j" 'magit-goto-next-section
      "k" 'magit-goto-previous-section
      "c" 'magit-commit
      "s" 'magit-stage-item
      "u" 'magit-unstage-item
      "p" 'magit-toggle-section
      "P" 'magit-push)
    (evil-define-key 'normal magit-log-mode-map
      "j" 'magit-goto-next-section
      "k" 'magit-goto-previous-section)
    (evil-define-key 'normal magit-diff-mode-map
      "j" 'magit-goto-next-section
      "k" 'magit-goto-previous-section)
    (evil-define-key 'normal magit-process-mode-map
      "j" 'evil-next-line
      "k" 'evil-previous-line)))

(defun my-ido-evil-keymaps ()
  "Keymaps for ido in evil mode."
  (progn
    (define-key ido-completion-map (kbd "l") 'ido-next-match)
    (define-key ido-completion-map (kbd "h") 'ido-prev-match)))

(use-package ido
  :demand evil
  :init (ido-mode t)
  :config
  (add-hook 'ido-minibuffer-setup-hook
            'my-ido-evil-keymaps))

(use-package evil
  :ensure t
  :commands (evil)
  :demand evil
  :demand swbuff-x
  :config
  (progn
    (setq evil-search-wrap nil)
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
    (my-dired-evil-keymaps)
    (my-package-list-evil-keymaps)
    (my-ibuffer-evil-keymaps)
    (evil-mode 1)))


(provide 'init)
;;; init.el ends here
