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

;; パス関連の設定
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (setq user-emacs-directory (abbreviate-file-name dir)))
(defvar e:custom-directory   (expand-file-name "custom/"   user-emacs-directory))
(defvar e:external-directory (expand-file-name "external/" user-emacs-directory))
(defvar e:private-directory  (expand-file-name "private/"  user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" e:custom-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spacemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; フォント設定の調整
(define-advice spacemacs/set-default-font (:after (&rest _) japanese-font-setting)
  (let ((font (car dotspacemacs-default-font)))
    (set-fontset-font t 'unicode (font-spec :family font))
    (set-variable 'face-font-rescale-alist (list font 1.00))
    (when (fboundp 'eaw-fullwidth)
      (eaw-fullwidth))))

(let ((user-emacs-directory (expand-file-name "spacemacs/" e:external-directory)))
  (setenv "SPACEMACSDIR" e:custom-directory)
  (load (expand-file-name "init" user-emacs-directory)))

(provide 'init)
;;; init.el ends here
