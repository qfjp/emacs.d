;;; package --- Summary
;;; Commentary:

;;; Code:

;; disable menubar, toolbar, and scrollbars
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Highlight cursor mode
(global-hl-line-mode)

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

;; Show trailing whitespace
(setq whitespace-style '(face trailing tabs tab-mark))
(global-whitespace-mode t)
(setq-default show-trailing-whitespace t)

;; set the default font
(set-frame-font "Fantasque Sans Mono-10")

;; Make emacs remember the last place in a file
(progn
  (setq save-place-file "~/.emacs.d/.place")
  (setq-default save-place t)
  (require 'saveplace))

;; Theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9f3a4edb56d094366afed2a9ba3311bbced0f32ca44a47a765d8ef4ce5b8e4ea")))
 '(safe-local-variable-values (quote ((folded-file . t))))
 '(when (not (facep (aref ansi-term-color-vector 0)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray19"))))
 '(linum ((t (:background "gray19" :foreground "#e0e0e0"))))
 '(linum-relative-current-face ((t (:inherit linum :background "color-18" :foreground "#CAE682" :weight bold))))
 '(show-paren-match ((t (:background "dim gray" :foreground "#202020")))))

;; Paren matching
(electric-pair-mode t)

;; Keymap configurations
(add-to-list 'load-path (concat user-emacs-directory "keymaps"))

(require 'my-ibuffer-keys)
(require 'my-package-list-keys)
(require 'my-dired-keys)
(require 'my-magit-keys)
(require 'my-ido-keys)

;; Plugin Configurations
(add-to-list 'load-path (concat user-emacs-directory "plug"))
(add-to-list 'load-path (concat user-emacs-directory "plug/modeline"))

(require 'my-package)
(require 'my-use-package)
(require 'my-env-setup)
(require 'my-swbuff-x)
(require 'my-helm)
(require 'my-linum)
(require 'my-company)
(require 'my-flycheck)
(require 'my-elisp-slime-nav)
(require 'my-fill-column-indicator)
(require 'my-magit)
(require 'my-ido)
(require 'my-folding)
(require 'my-latex)

;; Auto mode configuration
(add-to-list 'load-path (concat user-emacs-directory "automodes"))

(require 'my-conf-modes)
(require 'my-sh-modes)
(require 'my-markdown-modes)

;; theme
(use-package base16-theme
  :ensure t
  :init
  (load-theme 'base16-default-dark))

;; Rainbow delimiter
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(require 'my-evil)

(use-package sublimity
  :ensure t
  :config
  ;(require 'sublimity-scroll)
  ;(require 'sublimity-map)
  ;(require 'sublimity-attractive)
  (eval-after-load 'sublimity
    '(progn
       (evil-set-initial-state 'sublimity-mode 'normal)
       (evil-define-key 'normal sublimity-mode-map
         (kbd "j") 'evil-next-line
         (kbd "k") 'evil-previous-line)))
  (sublimity-mode t))
(require 'my-mode-line)

;; Undo tree history
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist
      '(("." . "~/.emacs.d/undo_hist")))

(custom-theme-set-faces
 'base16-default-dark
 `(cursor ((t (:foreground "#ffffff"))))
 )

(provide 'init)
;;; init.el ends here
