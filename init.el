;;; init.el --- Emacs initialization
;;; Commentary:

;;; Code:

;; disable menubar, toolbar, and scrollbars

;; Gui
(progn
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

;; Use primary instead of clipboard
(progn
  (setq x-select-enable-clipboard nil)
  (setq x-select-enable-primary t)
  (setq mouse-drag-copy-region t))

;; Highlight cursor mode
(global-hl-line-mode)

;; paren matching (highlight expression)
(show-paren-mode 1)
;;(setq show-paren-style 'expression)

;; stop prompting about git symlinks

;; Open symlinks in current directory without prompting
(setq vc-follow-symlinks nil)

;; Minibuffer persistent history
(savehist-mode 1)

;; disable start screen
(setq inhibit-splash-screen t)

;; spaces not tabs
(setq-default indent-tabs-mode nil)

;; disable backup and autosave
(progn
  (setq backup-inhibited t)
  (setq auto-save-default nil))

;; Emacs evil will use c-u to scroll
(setq evil-want-C-u-scroll t)

;; Show trailing whitespace
(progn
  (setq whitespace-style '(face trailing tabs tab-mark))
  (global-whitespace-mode t)
  (setq-default show-trailing-whitespace t))

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

;; Wrap long lines
(progn
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (global-visual-line-mode))

;; Keymap configurations
(add-to-list 'load-path (concat user-emacs-directory "config"))

(require 'keymaps/ibuffer)
(require 'keymaps/package-list)
(require 'keymaps/dired)
(require 'keymaps/magit)
(require 'keymaps/ido)
(require 'keymaps/eclim)
(require 'keymaps/org-agenda)
(require 'keymaps/project-explorer)

(require 'plug/package)
(require 'plug/use-package)
(require 'plug/env-setup)
(require 'plug/linum)
(require 'plug/irony)
(require 'plug/eclim)
(require 'plug/company)
(require 'plug/flycheck)
(require 'plug/elisp)
(require 'plug/column)
(require 'plug/magit)
(require 'plug/ido)
(require 'plug/folding-toggle)
(require 'plug/latex)
(require 'plug/project-search)
(require 'plug/paredit)
(require 'plug/guide-key)

;; org mode
(setq org-log-done 'time)
(add-hook 'org-mode-hook 'org-indent-mode)

(require 'automodes/conf)
(require 'automodes/sh)
(require 'automodes/markdown)

(use-package aggressive-indent
  :ensure t
  :init
  (global-aggressive-indent-mode 1)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode 'sh-mode))

(use-package emmet-mode
  :ensure t
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)))

(use-package indent-guide
  :ensure t
  :init
  (indent-guide-global-mode))

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

(require 'plug/evil)
(require 'plug/modeline)

;; Undo tree history
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist
      '(("." . "~/.emacs.d/undo_hist")))

(custom-theme-set-faces
 'base16-default-dark
 `(cursor ((t (:foreground "#ffffff")))))


(provide 'init)
;;; init.el ends here
