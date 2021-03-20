;;; dump-init.el
;;; Commentary:
;;; Code:

(let ((base (file-name-directory (or load-file-name buffer-file-name))))
  (require 'bootstrap (expand-file-name "bootstrap.el" base)))

(e:load-dump-init)

;;; dump-init.el ends here
