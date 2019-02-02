;;; init.el --- load this file at first when emacs was started.
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 各種設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn ;; パス設定
  (let ((emacs-dir (file-name-directory (or load-file-name buffer-file-name))))
    (setq user-emacs-directory (abbreviate-file-name emacs-dir)))
  (defvar e:custom-directory   (expand-file-name "custom/"   user-emacs-directory))
  (defvar e:external-directory (expand-file-name "external/" user-emacs-directory))
  (defvar e:private-directory  (expand-file-name "private/"  user-emacs-directory)))

(progn ;; フォント設定
  (defvar e:font-name "Ricty Diminished Discord")
  (defvar e:font-height 140)
  (defvar e:font-rescale 1.00))

(let ((private-config (expand-file-name "config.el" e:private-directory)))
  (when (file-exists-p private-config)
    (load-file private-config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spacemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defvar spacemacs-start-directory
    (expand-file-name "spacemacs/" e:external-directory)
    "Spacemacs start directory.")
  (setenv "SPACEMACSDIR" e:custom-directory)
  (load-file (expand-file-name "init.el" spacemacs-start-directory)))

(provide 'init)
;;; init.el ends here
