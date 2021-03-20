;;; early-init.el
;;; Commentary:
;;; Code:

(let ((base (file-name-directory (or load-file-name buffer-file-name))))
  (require 'bootstrap (expand-file-name "bootstrap.el" base)))

(e:load-early-init)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; early-init.el ends here
