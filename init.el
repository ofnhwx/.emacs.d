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

;; org ファイルをロードする設定
(when (require 'literate-elisp (expand-file-name "literate-elisp/literate-elisp.el" e:external-directory))
  (set-variable 'literate-elisp-begin-src-id "#+begin_src")
  (set-variable 'literate-elisp-end-src-id "#+end_src"))

;; 独自の変数・関数を定義
(literate-elisp-load-file (expand-file-name "user-variables.org" e:custom-directory))
(literate-elisp-load-file (expand-file-name "user-functions.org" e:custom-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spacemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((user-emacs-directory (expand-file-name "spacemacs/" e:external-directory)))
  (setenv "SPACEMACSDIR" e:custom-directory)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(provide 'init)
;;; init.el ends here
