;;; package --- Summary
;;; Commentary:
;;; Code:
(defvar-local git-branch-msg ""
  "The message to display in the mode line.")

(defvar-local git-branch ""
  "The name of the current git-branch.")

(defun get-git-branch ()
  "Get the name of the current git branch (or nil if not versioned)."
  (cond
   ((equalp (call-process-shell-command "git rev-parse --git-dir") 0)
    (setq git-branch-msg
          (concat
           "  "
           git-branch
           " ")))
   (t (setq git-branch-msg "")))
  git-branch-msg)

(defvar-local git-branch/buffer nil)
(defun set-branch-name ()
  "Set the branch name."
  (setq git-branch
        (substring
         (shell-command-to-string "git rev-parse --abbrev-ref HEAD")
         0 -1)))

(add-hook 'after-change-major-mode-hook #'set-branch-name)

(provide 'git-modeline)
;;; git-modeline.el ends here
