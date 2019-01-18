;;; init.el --- load this file at first when emacs was started.
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; パス設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((emacs-dir (file-name-directory (or load-file-name buffer-file-name))))
  (setq user-emacs-directory (abbreviate-file-name emacs-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spacemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defvar spacemacs-start-directory
    (expand-file-name "lisp/spacemacs/" user-emacs-directory)
    "Spacemacs start directory.")
  (setenv "SPACEMACSDIR" (expand-file-name "custom/" user-emacs-directory))
  (load-file (expand-file-name "init.el" spacemacs-start-directory)))

(provide 'init)
;;; init.el ends here
