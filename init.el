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

(progn
  (let ((emacs-dir (file-name-directory (or load-file-name buffer-file-name))))
    (setq user-emacs-directory (abbreviate-file-name emacs-dir)))
  (defvar e:lisp-directory   (expand-file-name "lisp/"   user-emacs-directory))
  (defvar e:custom-directory (expand-file-name "custom/" user-emacs-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spacemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defvar spacemacs-start-directory
    (expand-file-name "spacemacs/" e:lisp-directory)
    "Spacemacs start directory.")
  (setenv "SPACEMACSDIR" e:custom-directory)
  (load-file (expand-file-name "init.el" spacemacs-start-directory)))

(provide 'init)
;;; init.el ends here
