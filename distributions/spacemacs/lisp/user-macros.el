;;; user-macros.el

(defmacro e:progn! (package &rest body)
  (declare (indent 1))
  `(progn ,@body))

(defmacro e:after! (package &rest body)
  (declare (indent 1))
  `(with-eval-after-load ',package
     (progn ,@body)))

(defmacro e:default! (variable default)
  `(setq-default ,variable ,default))

(defmacro e:local! (variable value)
  `(setq-local ,variable ,value))

(defmacro e:variable! (variable value)
  `(set-variable ',variable ,value))

(defmacro e:cache! (variable path)
  `(let ((path (expand-file-name ,path spacemacs-cache-directory)))
     (unless (f-exists? (f-parent path))
       (make-directory (f-parent path) t))
     (set-variable ',variable path)))

(provide 'user-macros)

;;; user-macros.el ends here
