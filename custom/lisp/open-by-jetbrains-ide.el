
(defconst jetbrains:idea
  (cond
   ((e:system-type-darwin-p)
    '("/Applications/IntelliJ IDEA.app" "/Applications/IntelliJ IDEA.app/Contents/MacOS/idea"))
   (t nil))
  "IntelliJ IDEAのパス.")

(defconst jetbrains:clion
  (cond
   ((e:system-type-darwin-p)
    '("/Applications/CLion.app" "/Applications/CLion.app/Contents/MacOS/clion"))
   (t nil))
  "CLionのパス.")

(defun jetbrains:select-app (buffer)
  (with-current-buffer buffer
    (cond
     ((member major-mode '(c++-mode)) intellij:clion-path)
     (t nil))))



(defun jetbrains:make-commands (app buffer)
  (let ((commands nil))
    (with-current-buffer buffer
      (cond
       ((e:system-type-darwin-p)
        (add-to-list 'commands
                     (format "open -a %s"
                             (shell-quote-argument (nth 0 app))))
        (add-to-list 'commands
                     (format "%s --line %d %s >/dev/null 2>&1"
                             (shell-quote-argument (nth 1 app))
                             (line-number-at-pos)
                             (buffer-file-name))))))
    commands))



(defun jetbrains:open-by-idea (&optional buffer)
  (interactive)
  (jetbrains:open-by-ide jetbrains:idea buffer))

(defun jetbrains:open-by-clion (&optional buffer)
  (interactive)
  (jetbrains:open-by-ide jetbrains:clion buffer))

(defun jetbrains:open-by-ide (&optional app buffer)
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (unless app
    (setq app (jetbrains:select-app buffer)))
  (let ((commands (jetbrains:make-commands app buffer)))
    (cl-dolist (command commands)
      (shell-command command))))



(provide 'open-by-jetbrains-ide)
