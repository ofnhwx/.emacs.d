;;; nsemacs/bootstrap.el
;;; Commentary:
;;; Code:

(defun e:load-early-init ()
  (let ((user-emacs-directory (expand-file-name "nsmacs/" e:external-directory)))
    (load (expand-file-name "early-init" user-emacs-directory))))

(defun e:load-init ()
  (let ((user-emacs-directory (expand-file-name "nsmacs/" e:external-directory)))
    (load (expand-file-name "init" user-emacs-directory))))

;;; doom/bootstrap.el ends here
