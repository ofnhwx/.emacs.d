;;; user-macro.el

(defmacro e:place-in-cache (variable path)
  "VARIABLE のファイルを PATH に指定した名称でキャッシュディレクトリに設定する."
  `(let ((path (expand-file-name ,path spacemacs-cache-directory)))
     (unless (f-exists? (f-parent path))
       (make-directory (f-parent path) t))
     (set-variable ',variable path)))

(provide 'user-macro)

;;; user-macro.el ends here
