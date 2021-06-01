;;; yas-rails-helper.el

(eval-and-compile
  (require 'komunan-lisp-library)
  (require 'cl-lib)
  (require 'dash)
  (require 'f)
  (require 's))

(defun yas:rails/current-path ()
  (string-trim-left (f-short buffer-file-name)
                    (regexp-quote (kllib:project-root buffer-file-name))))

(defun yas:rails/current-namespace ()
  (yas:rails/namespace (yas:rails/current-path)))

(defun yas:rails/current-class ()
  (yas:rails/class (yas:rails/current-path)))

(defun yas:rails/current-model ()
  (yas:rails/model (yas:rails/current-path)))

(defun yas:rails/namespace (path)
  (let* ((seq (f-split path))
         (subseq (cl-subseq seq 1 (1- (length seq)))))
    (s-join "::" (--map (kllib:convert it :camelize) subseq))))

(defun yas:rails/class (path)
  (let* ((seq (f-split path))
         (filename (string-trim-right (-last-item seq) "\\.rb$")))
    (kllib:convert filename :camelize)))

(defun yas:rails/model (path)
  (let ((class (yas:rails/class path)))
    (kllib:convert (string-trim-right class "Controller$") :classify)))

(provide 'yas-rails-helper)

;;; yas-rails-helper.el ends here
