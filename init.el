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
  (require 'custom-paths (expand-file-name "custom-paths.el" dir))
  (add-to-list 'load-path (expand-file-name "lisp" e:custom-directory)))

;; 最新の org を使用する
(let ((dir (expand-file-name "org-mode" e:external-directory)))
  (when (file-directory-p dir)
    (add-to-list 'load-path (expand-file-name "lisp" dir))
    (add-to-list 'load-path (expand-file-name "contrib" dir))))

;; フォント設定
(progn
  (defvar e:font-name "Cica")
  (defvar e:font-size (cond
                       ((eq system-type 'darwin) 14)
                        (t 16)))
  (defvar e:font-rescale 1.00)
  (defun e:font ()
    (list e:font-name :size e:font-size))
  (define-advice spacemacs/set-default-font (:after (&rest _) japanese-font-setting)
    (set-fontset-font t 'unicode (font-spec :family e:font-name))
    (set-variable 'face-font-rescale-alist (list e:font-name e:font-rescale))
    (when (fboundp 'eaw-fullwidth)
      (eaw-fullwidth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spacemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((user-emacs-directory (expand-file-name "spacemacs/" e:external-directory)))
  (setenv "SPACEMACSDIR" e:custom-directory)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(provide 'init)
;;; init.el ends here
