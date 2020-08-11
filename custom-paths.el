;;; custom-paths.el
;;; Commentary:
;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (setq user-emacs-directory (abbreviate-file-name dir)))

(defvar e:custom-directory   (expand-file-name "custom/"   user-emacs-directory))
(defvar e:external-directory (expand-file-name "external/" user-emacs-directory))
(defvar e:private-directory  (expand-file-name "private/"  user-emacs-directory))

(provide 'custom-paths)
;;; custom-paths.el ends here
