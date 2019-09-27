
(require 'cl-lib)
(require 'dash)



(defconst jetbrains/ide-alist
  '(
    ("Android Studio" . jetbrains/ide-studio)
    ("AppCode"        . jetbrains/ide-appcode)
    ("CLion"          . jetbrains/ide-clion)
    ("GoLand"         . jetbrains/ide-goland)
    ("IntelliJ IDEA"  . jetbrains/ide-idea)
    ("PhpStorm"       . jetbrains/ide-pstorm)
    ("PyCharm"        . jetbrains/ide-charm)
    ("Rider"          . jetbrains/ide-rider)
    ("RubyMine"       . jetbrains/ide-mine)
    ("WebStorm"       . jetbrains/ide-wstorm)
    ))

(defvar jetbrains/use-toolbox-mode nil)

(defvar jetbrains/ide-appcode "appcode")
(defvar jetbrains/ide-charm   "charm")
(defvar jetbrains/ide-clion   "clion")
(defvar jetbrains/ide-goland  "goland")
(defvar jetbrains/ide-idea    "idea")
(defvar jetbrains/ide-mine    "mine")
(defvar jetbrains/ide-pstorm  "pstorm")
(defvar jetbrains/ide-rider   "rider")
(defvar jetbrains/ide-studio  "studio")
(defvar jetbrains/ide-wstorm  "wstorm")

(progn
  (defvar jetbrains/need-open-confirm nil)
  (put 'jetbrains/need-open-confirm 'safe-local-variable 'booleanp))

(progn
  (defvar-local jetbrains/ide nil)
  (put 'jetbrains/ide 'safe-local-variable 'jetbrains/--ide))



;;;###autoload
(defun jetbrains/open-by-appcode (&optional buffer)
  (interactive)
  (jetbrains/open-by-ide "AppCode" buffer))

;;;###autoload
(defun jetbrains/open-by-charm (&optional buffer)
  (interactive)
  (jetbrains/open-by-ide "PyCharm" buffer))

;;;###autoload
(defun jetbrains/open-by-clion (&optional buffer)
  (interactive)
  (jetbrains/open-by-ide "CLion" buffer))

;;;###autoload
(defun jetbrains/open-by-goland (&optional buffer)
  (interactive)
  (jetbrains/open-by-ide "GoLand" buffer))

;;;###autoload
(defun jetbrains/open-by-idea (&optional buffer)
  (interactive)
  (jetbrains/open-by-ide "IntelliJ IDEA" buffer))

;;;###autoload
(defun jetbrains/open-by-mine (&optional buffer)
  (interactive)
  (jetbrains/open-by-ide "RubyMine" buffer))

;;;###autoload
(defun jetbrains/open-by-pstorm (&optional buffer)
  (interactive)
  (jetbrains/open-by-ide "PhpStorm" buffer))

;;;###autoload
(defun jetbrains/open-by-rider (&optional buffer)
  (interactive)
  (jetbrains/open-by-ide "Rider" buffer))

;;;###autoload
(defun jetbrains/open-by-studio (&optional buffer)
  (interactive)
  (jetbrains/open-by-ide "Android Studio" buffer))

;;;###autoload
(defun jetbrains/open-by-wstorm (&optional buffer)
  (interactive)
  (jetbrains/open-by-ide "WebStorm" buffer))

;;;###autoload
(defun jetbrains/open-by-ide (&optional ide buffer)
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (ide (or (jetbrains/--ide ide)
                  (jetbrains/--ide-select buffer)))
         (commands (if jetbrains/use-toolbox-mode
                       (jetbrains/--make-toolbox-commands ide buffer)
                     (jetbrains/--make-commands ide buffer))))
    (cond
     ((not (buffer-file-name buffer))
      (message "Buffer `%s' has no file name." buffer))
     ((not commands)
      (message "%s is not found." ide))
     ((or (not jetbrains/need-open-confirm)
          (y-or-n-p (format "Do you want to open `%s'?" ide)))
      (cl-dolist (command commands)
        (shell-command command))))))



(defun jetbrains/--ide (name)
  (cond
   ((symbolp name)
    (car (--first (eq name (cdr it)) jetbrains/ide-alist)))
   ((stringp name)
    (car (--first (string-equal name (car it)) jetbrains/ide-alist)))))

(defun jetbrains/--ide-select (buffer)
  (or (jetbrains/--ide jetbrains/ide)
      (with-current-buffer buffer
        (cond
         ((member major-mode '(php-mode))
          "PhpStorm")
         ((member major-mode '(c++-mode))
          "CLion")
         (t
          "IntelliJ IDEA")))))

(defun jetbrains/--ide-executable (ide)
  (let* ((sym (cdr (--first (string-equal ide (car it)) jetbrains/ide-alist)))
         (val (symbol-value sym)))
    (and val
         (executable-find val))))

(defun jetbrains/--project-root (filename)
  (let ((dir (file-name-directory filename)))
    (or (locate-dominating-file dir ".idea/")
        (cl-first (--sort (> (length it) (length other))
                          (--map (locate-dominating-file dir it)
                                 '(".git/" ".hg/" ".svn/" ".git")))))))

(defun jetbrains/--make-commands (ide buffer)
  (let* ((executable (jetbrains/--ide-executable ide))
         (filename (buffer-file-name buffer))
         (project (and filename (or (let ((root (jetbrains/--project-root filename)))
                                      (expand-file-name root))
                                    "--temp-project"))))
    (when (and executable filename)
      ;; {{application}} [-l|--line line] [project_dir|--temp-project] file[:line]
      (list (format "%s %s %s:%d"
                    (shell-quote-argument executable)
                    (shell-quote-argument project)
                    (shell-quote-argument filename)
                    (with-current-buffer buffer (line-number-at-pos)))))))

(defun jetbrains/--make-toolbox-commands (ide buffer)
  (let* ((filename (buffer-file-name buffer))
         (matched (s-match
                   "open[[:space:]]+-a[[:space:]]+\"\\(\\(.*?\\.app\\).*?\\)\""
                   (with-temp-buffer
                     (ignore-errors (insert-file-contents (jetbrains/--ide-executable ide)))
                     (buffer-string))))
         (executable (nth 1 matched))
         (application (nth 2 matched)))
    (when (and executable application filename)
      (list (format "%s --line %d %s >/dev/null 2>&1"
                    (shell-quote-argument executable)
                    (with-current-buffer buffer (line-number-at-pos))
                    (shell-quote-argument filename))
            (format "open -a %s"
                    (shell-quote-argument executable))))))



(provide 'open-by-jetbrains-ide)
