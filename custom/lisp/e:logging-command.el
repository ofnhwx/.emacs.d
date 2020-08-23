
(eval-and-compile
  (require 'e:convenient-features)
  (require 's))

(defun e:logging--output (fn message)
  (e:message "[%s] %s" (propertize fn 'font-lock-face '(:foreground "#00bfff")) message))

(defun e:logging--start-process (name buffer program &rest args)
  (e:logging--output "start-process" (format "%s %s" program (s-join " " args))))

(defun e:logging--call-process (program &optional infile destination display &rest args)
  (e:logging--output "call-process" (format "%s %s" program (s-join " " args))))

(defun e:logging-command-on ()
  (interactive)
  (advice-add 'start-process :before #'e:logging--start-process)
  (advice-add 'call-process  :before #'e:logging--call-process))

(defun e:logging-command-off ()
  (interactive)
  (advice-remove 'start-process #'e:logging--start-process)
  (advice-remove 'call-process  #'e:logging--call-process))

(provide 'e:logging-command)
