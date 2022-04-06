;;; init.el --- load this file at first when emacs was started.
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;; https://emacs-jp.slack.com/archives/C1B5WTJLQ/p1614558372038400
(when (string-equal emacs-version "28.1.50")
  (set-variable 'warning-suppress-log-types '((comp)))
  (set-variable 'warning-suppress-types '((comp))))

(let ((base (file-name-directory (or load-file-name buffer-file-name))))
  (require 'bootstrap (expand-file-name "bootstrap.el" base)))

(e:load-init)

(provide 'init)
;;; init.el ends here
