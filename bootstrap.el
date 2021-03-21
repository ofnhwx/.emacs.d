;;; bootstrap.el ---
;;; Commentary:
;;; Code:

(defvar e:distribution (or (getenv "EMACS_DISTRIBUTION") "spacemacs"))

(let ((base (file-name-directory (or load-file-name buffer-file-name))))
  (setq user-emacs-directory (abbreviate-file-name base)))

(defconst e:external-directory      (expand-file-name "external/"      user-emacs-directory))
(defconst e:private-directory       (expand-file-name "private/"       user-emacs-directory))
(defconst e:distributions-directory (expand-file-name "distributions/" user-emacs-directory))
(defconst e:distribution-directory  (expand-file-name e:distribution e:distributions-directory))

(add-to-list 'load-path (expand-file-name "lisp" e:distribution-directory))

(load (expand-file-name "bootstrap" e:distribution-directory))

(provide 'bootstrap)

;;; bootstrap.el ends here
