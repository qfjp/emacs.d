;;; plug/my-use-package.el --- Auto install packages on first use
;;; Commentary:
;;; Code:
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(provide 'plug/my-use-package)
;;; plug/my-use-package ends here
