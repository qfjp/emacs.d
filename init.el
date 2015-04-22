;;; package --- Summary
;;; Commentary:

;;; Code:

;; disable menubar, toolbar, and scrollbars
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq inhibit-splash-screen t)

;; spaces not tabs
(setq-default indent-tabs-mode nil)

;; disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Emacs evil will use c-u to scroll
(setq evil-want-C-u-scroll t)

;; disable search auto-wrap
;;(setq isearch-wrap-function '(lambda nil))

;; set the default font
(set-frame-font "Fantasque Sans Mono-10")

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

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
            'my-ido-evil-keymaps)
  )

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x))

(use-package linum-relative
  :ensure t
  :demand linum-relative
  :config
  (add-hook 'after-change-major-mode-hook 'linum-mode))

;;(use-package auto-complete
;;  :ensure t
;;  :demand auto-complete
;;  :init
;;  (ac-config-default)
;;  (require 'auto-complete-config)
;;  :config
;;  (define-key ac-complete-mode-map (kbd "C-n") 'ac-expand)
;;  (define-key ac-menu-map (kbd "C-b") 'ac-previous)
;;  (define-key ac-menu-map (kbd "C-p") 'ac-previous))
(use-package ac-helm
  :ensure t)
(use-package auto-complete
  :ensure t
  :demand ac-helm
  :init
  (ac-config-default)
  :config
  (global-set-key (kbd "C-n") 'ac-complete-with-helm)
  (define-key ac-complete-mode-map (kbd "C-n") 'ac-complete-with-helm))

(use-package jedi
  :ensure t
  :demand jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (setq jedi:complete-on-dot t))

;;(eval-after-load 'helm
;;  '(progn
;;     (evil-set-initial-state 'helm-mode 'normal)
;;     (evil-define-key 'normal helm-mode-map
;;       (kbd "j") 'helm-next-line)))

(defun my-ibuffer-evil-keymaps ()
  "Keymaps for ibuffer in evil mode."
  (eval-after-load 'ibuffer
    '(progn
       (evil-set-initial-state 'ibuffer-mode 'normal)
       (evil-define-key 'normal ibuffer-mode-map
         (kbd "J") 'ibuffer-jump-to-buffer
         (kbd "j") 'evil-next-line
         (kbd "k") 'evil-previous-line
         (kbd "l") 'ibuffer-visit-buffer
         (kbd "v") 'ibuffer-toggle-marks))))

;; Package menu mode
(defun my-package-list-evil-keymaps ()
  "Keymaps for package in evil mode."
  (eval-after-load 'package
    '(progn
       (evil-set-initial-state 'package-menu-mode 'normal)
       (evil-define-key 'normal package-menu-mode-map
         (kbd "d") 'package-menu-mark-delete
         (kbd "K") 'package-menu-describe-package
         (kbd "i") 'package-menu-mark-install
         (kbd "x") 'package-menu-execute
         (kbd "j") 'evil-next-line
         (kbd "k") 'evil-previous-line))))

;; dired
(defun my-dired-evil-keymaps ()
  "Evil keymaps for dired-x."
  (require 'dired-x)
  (put 'dired-find-alternate-file 'disabled nil)
  (eval-after-load 'dired
    '(progn
       (evil-set-initial-state 'dired-mode 'normal)
       (defun my-dired-up-directory ()
         "Take dired up one directory, but behave like
                        dired-find-alternate-file."
         (interactive)
         (let ((old (current-buffer)))
           (dired-up-directory)
           (kill-buffer old)))
       (evil-define-key 'normal dired-mode-map "h"
         'my-dired-up-directory)
       (evil-define-key 'normal dired-mode-map "l"
         'dired-find-alternate-file)
       (evil-define-key 'normal dired-mode-map "o"
         'dired-sort-toggle-or-edit)
       (evil-define-key 'normal dired-mode-map "v"
         'dired-toggle-marks)
       (evil-define-key 'normal dired-mode-map "m" 'dired-mark)
       (evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
       (evil-define-key 'normal dired-mode-map "U"
         'dired-unmark-all-marks)
       (evil-define-key 'normal dired-mode-map "c"
         'dired-create-directory)
       (evil-define-key 'normal dired-mode-map "n"
         'evil-search-next)
       (evil-define-key 'normal dired-mode-map "N"
         'evil-search-previous)
       (evil-define-key 'normal dired-mode-map "q"
         'kill-this-buffer))))


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
  (evil-leader/set-key "SPC" 'evil-search-highlight-persist-remove-all))

(use-package powerline-evil
  :ensure t
  :demand evil
  :init
  (powerline-evil-vim-color-theme)
  :config
  (display-time-mode t))


;; Flycheck
(use-package flycheck
  :ensure t
  :demand flycheck
  :init
  (global-flycheck-mode t)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;;; flycheck errors on tooltip - not in console :-(
;;;(when (display-graphic-p (selected-frame))
;;;  (eval-after-load 'flycheck
;;;    '(custom-set-variables
;;;      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))

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
 '(custom-safe-themes
   (quote
    ("261f5ddd72a1c0b47200e13d872075af5f78c3f07d2968bddc0301261934f210"
     "8022cea21aa4daca569aee5c1b875fbb3f3248a5debc6fc8cf5833f2936fbb22"
     default))))
(custom-set-faces)
(load-theme 'base16-default-dark)

;; Fill column indicator
(use-package fill-column-indicator
  :ensure t
  :demand fill-column-indicator
  :config
  (add-hook 'after-change-major-mode-hook 'fci-mode)
  (setq fci-rule-column 70))

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
    (evil-define-key 'normal magit-mode-map
      "j" 'magit-goto-next-section
      "k" 'magit-goto-previous-section)
    (evil-define-key 'normal magit-log-mode-map
      "j" 'magit-goto-next-section
      "k" 'magit-goto-previous-section)
    (evil-define-key 'normal magit-diff-mode-map
      "j" 'magit-goto-next-section
      "k" 'magit-goto-previous-section)))

(use-package evil
  :ensure t
  :commands (evil)
  :demand evil
  :config
  (progn
    (setq evil-search-wrap nil)
    (define-key evil-normal-state-map (kbd "gh") 'help-command)
    (define-key key-translation-map (kbd "gx") (kbd "C-x"))
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-normal-state-map (kbd ";") 'evil-ex)
    (define-key evil-ex-map (kbd "w ;") 'save-buffer) ; quick save
    (my-dired-evil-keymaps)
    (my-package-list-evil-keymaps)
    (my-ibuffer-evil-keymaps)
    (evil-mode 1)))

(provide 'init)
;;; init.el ends here
