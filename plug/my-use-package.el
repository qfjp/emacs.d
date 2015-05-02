;;; package --- Summary
;;; Commentary:
;;; Code:
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(provide 'my-use-package)
;;; my-use-package ends here
