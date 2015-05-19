;;; plug/evil.el --- EVIL EVIL EVIL
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


(define-prefix-command 'lisp-prefix)
(define-prefix-command 'project-prefix)
(define-prefix-command 'org-prefix)

(use-package evil-leader
  :commands (evil-leader-mode)
  :ensure t
  :ensure paredit
  :ensure projectile
  ;:ensure project-explorer
  :demand evil-leader
  :init
  (global-evil-leader-mode)
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "w" 'save-buffer
      "q" 'kill-buffer-and-window
      "b" 'ido-switch-buffer
      ;; projectile
      "p" 'project-prefix
      "p b" 'projectile-switch-to-buffer
      "p D" 'projectile-dired
      "p d" 'projectile-find-dir
      "p e" 'project-explorer-open
      "p f" 'projectile-find-file
      "p j" 'projectile-find-tag
      "p k" 'projectile-kill-buffers
      "p R" 'projectile-regenerate-tags

      ;; paredit
      "l" 'lisp-prefix
      "l j" 'paredit-splice-sexp
      "l k" 'paredit-wrap-round
      "l r" 'paredit-raise-sexp
      "l C" 'paredit-convolute-sexp
      "l s" 'paredit-forward-slurp-sexp
      "l S" 'paredit-backward-slurp-sexp
      "l b" 'paredit-forward-barf-sexp
      "l B" 'paredit-backward-barf-sexp
      ;;"p r" 'helm-projectile-recentf
      ;;"p s" 'helm-projectile-switch-project

      ;; org-mode
      "o" 'org-prefix
      "o t" 'org-todo
      "o T" 'org-insert-todo-heading
      "o l" 'org-store-link
      "o L" 'org-insert-link
      "o g" 'org-todo-list

      )))

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

(defun evil/set-key (state key def &rest bindings)
  (define-key state (kbd key) def)
  (cond ((null bindings)
         nil)
        (t
         (if (equal (length bindings) 1)
             (setq bindings (first bindings)))
         (let ((next-key (first bindings))
               (next-def (first (rest bindings)))
               (next-rest (rest (rest bindings))))
           (if (null next-rest)
               (evil/set-key state next-key next-def)
             (evil/set-key state next-key next-def next-rest))))))

(use-package evil
  :ensure t
  :commands (evil)
  :demand evil
  :demand swbuff-x
  :config
  (progn
    (setq evil-search-wrap nil)
    (evil/set-key evil-normal-state-map
                  "j" 'evil-next-visual-line
                  "k" 'evil-previous-visual-line
                  "gj" 'evil-next-line
                  "gk" 'evil-previous-line
                  "gh" 'help-command
                  "ge" 'eval-defun
                  "C-h" 'evil-window-left
                  "C-j" 'evil-window-down
                  "C-k" 'evil-window-up
                  "C-l" 'evil-window-right
                  "C-n" 'ido-switch-buffer
                  "C-b" 'ido-switch-buffer
                  "z[" 'hs-show-all
                  "z]" 'hs-hide-all
                  ";" 'evil-ex)
    (define-key key-translation-map (kbd "gx") (kbd "C-x"))
    (evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
      'elisp-slime-nav-describe-elisp-thing-at-point)
    (define-key evil-ex-map (kbd "w ;") 'save-buffer) ; quick save

    (my-setup-folding-by-marks)
    ;;(add-hook 'evil-insert-state-entry-hook (lambda () (linum-mode -1)))
    ;;(add-hook 'evil-insert-state-exit-hook (lambda () (linum-mode)))
    (keymaps/dired)
    (keymaps/package-list)
    (keymaps/ibuffer)
    (keymaps/magit)
    (keymaps/org-agenda)
    (evil-mode 1)))

(provide 'plug/evil)
;;; plug/evil.el ends here
