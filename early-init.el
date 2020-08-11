;;; early-init.el
;;; Commentary:
;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (require 'custom-paths (expand-file-name "custom-paths.el" dir)))

(load-file (expand-file-name "spacemacs/early-init.el" e:external-directory))

;;; early-init.el ends here
