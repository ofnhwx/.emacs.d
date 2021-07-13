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

(defun orderless-migemo (component)
  (let ((pattern (migemo-get-pattern component)))
    (condition-case nil
        (progn (string-match-p pattern "") pattern)
      (invalid-regexp nil))))

(defun org-support/weekday-in-month (month year &optional day-of-week)
  (with-no-warnings
    (defvar date)
    (defvar entry))
  (require 'japanese-holidays)
  (let ((y (calendar-extract-year  date))
        (m (calendar-extract-month date))
        (day-of-week (cond
                      ((null day-of-week) '(1 2 3 4 5))
                      ((listp day-of-week) day-of-week)
                      (t (list day-of-week)))))
    (and (= y year)
         (= m month)
         (message "1")
         (memq (calendar-day-of-week date) day-of-week)
         (not (calendar-check-holidays date)))))
