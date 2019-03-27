
(require 'cl-lib)
(require 'dash)



(defconst jetbrains:idea
  (executable-find "idea")
  "IntelliJ IDEAのパス.")

(defconst jetbrains:pstorm
  (executable-find "pstorm")
  "PhpStormのパス.")

(defconst jetbrains:clion
  (executable-find "clion")
  "CLionのパス.")

(defun jetbrains:select-app (buffer)
  (with-current-buffer buffer
    (cond
     ((member major-mode '(php-mode)) jetbrains:pstorm)
     ((member major-mode '(c++-mode)) jetbrains:clion)
     (t jetbrains:idea))))



(defun jetbrains:project-root (buffer)
  (let ((dir (file-name-directory (buffer-file-name buffer))))
    (cl-first (--sort (> (length it) (length other))
                      (--map (locate-dominating-file dir it)
                             '(".git/" ".hg/" ".svn/" ".git"))))))

(defun jetbrains:make-command (app buffer)
  (when app
    (with-current-buffer buffer
      ;; {{application}} [-l|--line line] [project_dir|--temp-project] file[:line]
      (format "%s %s %s:%d"
              (shell-quote-argument app)
              (or (jetbrains:project-root buffer)
                  "--temp-project")
              (abbreviate-file-name (buffer-file-name))
              (line-number-at-pos)))))



(defun jetbrains:open-by-idea (&optional buffer)
  (interactive)
  (jetbrains:open-by-ide jetbrains:idea buffer))

(defun jetbrains:open-by-pstorm (&optional buffer)
  (interactive)
  (jetbrains:open-by-ide jetbrains:pstorm buffer))

(defun jetbrains:open-by-clion (&optional buffer)
  (interactive)
  (jetbrains:open-by-ide jetbrains:clion buffer))

(defun jetbrains:open-by-ide (&optional app buffer)
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (app (or app (jetbrains:select-app buffer)))
         (command (jetbrains:make-command app buffer)))
    (when command
      (shell-command command))))



(provide 'open-by-jetbrains-ide)
