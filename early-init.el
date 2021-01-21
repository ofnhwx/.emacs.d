;;; early-init.el
;;; Commentary:
;;; Code:

(let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
       (user-emacs-directory (expand-file-name "external/spacemacs/" current-dir)))
  (load (expand-file-name "early-init" user-emacs-directory)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; early-init.el ends here
