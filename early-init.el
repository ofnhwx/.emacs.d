;;; early-init.el
;;; Commentary:
;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "external/spacemacs/early-init" dir)))

;;; early-init.el ends here
