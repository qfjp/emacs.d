;;; plug/use-package.el --- Auto install packages on first use
;;; Commentary:
;;; Code:
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(provide 'plug/use-package)
;;; plug/use-package ends here
