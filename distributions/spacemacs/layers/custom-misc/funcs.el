;;; funcs.el

(defun cov--locate-simplecov (file-dir file-name)
  (let ((dir (kllib:project-root file-dir)))
    (when dir
      (cons (f-expand "coverage/.resultset.json" dir) 'simplecov))))

(defun cov--simplecov-parse ()
  (eval-when-compile
    (defvar cov-coverage-file))
  (let* ((contents (buffer-string))
         (coverage (let-alist (json-parse-string contents :object-type 'alist :array-type 'list)
                     .RSpec.coverage))
         (project-root (f-expand (kllib:project-root cov-coverage-file))))
    (-map (lambda (item)
            (let ((file (symbol-name (first item)))
                  (list (cdadr item)))
              (cons (s-replace project-root "" file)
                    (->> list
                      (--map-indexed (list (1+ it-index) it))
                      (--reject (eq (second it) :null))))))
          coverage)))
