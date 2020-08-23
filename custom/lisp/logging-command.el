
(eval-and-compile
  (require 's))

(defun logging--output (fn message)
  (let ((inhibit-read-only t))
    (with-current-buffer (messages-buffer)
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (format "[%s] %s" (propertize fn 'font-lock-face '(:foreground "#00bfff")) message))
      (unless (bolp)
        (insert "\n")))))

(defun logging--start-process (name buffer program &rest args)
  (logging--output "start-process" (format "%s %s" program (s-join " " args))))

(defun logging--call-process (program &optional infile destination display &rest args)
  (logging--output "call-process" (format "%s %s" program (s-join " " args))))

(defun logging-command-on ()
  (interactive)
  (advice-add 'start-process :before #'logging--start-process)
  (advice-add 'call-process  :before #'logging--call-process))

(defun logging-command-off ()
  (interactive)
  (advice-remove 'start-process #'logging--start-process)
  (advice-remove 'call-process  #'logging--call-process))

(provide 'logging-command)
