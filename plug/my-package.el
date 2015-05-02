;;; my-package --- Summary
;;; Commentary:
"Setup for package directories in emacs init."

;;; Code:
(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(provide 'my-package)
;;; package.el ends here
