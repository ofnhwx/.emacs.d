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

(defun at-point-string-p (&optional point)
  (let ((point (or point (point))))
    (and (>= point (point-min))
         (<= point (point-max))
         (eq 'string (syntax-ppss-context (syntax-ppss point))))))

(defun string-range-at-point ()
  (let ((s (point))
        (e (point)))
    (when (at-point-string-p)
      (while (at-point-string-p s)
        (cl-decf s))
      (while (at-point-string-p e)
        (cl-incf e))
      (when (eq (char-after e) #x0a)
        (cl-decf e))
      (list s e (buffer-substring s e)))))
