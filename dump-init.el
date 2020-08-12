;;; dump-init.el
;;; Commentary:
;;; Code:

(let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
       (spacemacs-init      (expand-file-name "external/spacemacs/init.el"   dir))
       (spacemacs-dump-init (expand-file-name "external/spacemacs/dump-init" dir))
       (custom-init         (expand-file-name "init" dir)))
  (defun fake-load-with-dump (args)
    (if (string-equal (car args) spacemacs-init)
        (prog1 (list custom-init)
          (advice-remove 'load #'fake-load-with-dump))
      args))
  (advice-add 'load :filter-args #'fake-load-with-dump)
  (load spacemacs-dump-init))

;;; dump-init.el ends here
