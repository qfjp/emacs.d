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
(setq save-place-file "~/.emacs.d/.place")
(setq-default save-place t)
(require 'saveplace)


;; Zsh syntax highlighting
(add-hook 'sh-mode-hook
          (lambda ()
            (if (string-match "\\.zsh$" buffer-file-name)
                (sh-set-shell "zsh"))))

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
;;(require 'my-autocomplete)
(require 'my-company)
(require 'my-flycheck)
(require 'my-elisp-slime-nav)
(require 'my-fill-column-indicator)
(require 'my-magit)
(require 'my-ido)

;; Auto mode configuration
(add-to-list 'load-path (concat user-emacs-directory "automodes"))

(require 'my-conf-modes)
(require 'my-sh-modes)
(require 'my-markdown-modes)

(defun my-setup-folding-by-marks ()
  "Setup folding based on markers."
  (load "folding" 'nomessage 'noerror)
  (folding-mode-add-find-file-hook)
  (folding-add-to-marks-list 'sh-mode "# {{{" "# }}}")

  (defun my-change-fold-keymaps ()
    (if folded-file
        (progn
          (define-key evil-normal-state-local-map (kbd "z[")
            'folding-open-buffer)
          (define-key evil-normal-state-local-map (kbd "z]")
            'folding-whole-buffer)
          (define-key evil-normal-state-local-map (kbd "zo")
            'folding-show-current-entry)
          (define-key evil-normal-state-local-map (kbd "zc")
            'folding-hide-current-entry))))
  (defadvice folding-mode (after folding-mode activate)
    (my-change-fold-keymaps))
  (ad-activate 'folding-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))

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

;; Guide key

;; Auctex
(use-package tex-site
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-view-program-selection
        '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
        '(("PDF Viewer" "zathura %o"))))

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
