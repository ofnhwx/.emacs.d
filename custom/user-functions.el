
(progn ;; フォント設定
  (defvar e:font-name "Ricty Diminished Discord")
  (defvar e:font-size 14)
  (defvar e:font-rescale 1.00)
  (defun e:font ()
    "フォント"
    (list e:font-name :size e:font-size)))

(progn ;; プロジェクトルートの取得
  (defvar e:project-root-mark '(".git/" ".hg/" ".svn/" ".git")))

(defun e:auth-source-get (property &rest spec)
  "認証情報から SPEC に一致する項目の PROPERTY を取得する."
  (let ((plist (car (apply 'auth-source-search spec)))
        (pkey (intern (format ":%s" property))))
    (when plist
      (plist-get plist pkey))))

(defun e:system-type-darwin-p ()
  "OS が Mac かを取得する."
  (eq system-type 'darwin))

(defun e:remove-nth (n list)
  "N 番目の要素を LIST から取り除いて返す."
  (if (or (zerop n) (null list))
      (cdr list)
    (cons (car list) (e:remove-nth (1- n) (cdr list)))))

(defun e:project-root (dir)
  "指定した DIR のプロジェクトルートを返す."
  (let* ((dir (file-name-directory (expand-file-name dir)))
         (candidates (mapcar (lambda (name) (locate-dominating-file dir name)) e:project-root-mark))
         (root (car (sort candidates (lambda (this other) (> (length this) (length other)))))))
    (if root (abbreviate-file-name root) nil)))

(defun e:project-name (root)
  "指定した ROOT のプロジェクトの名前をいい感じに返す."
  (let* ((root (directory-file-name root))
         (name (file-name-nondirectory root))
         (parent (e:project-root root)))
    (if parent
        (concat (e:project-name parent) "/" name)
      name)))

(defun e:shorten (path &optional max separator omit)
  "指定された FILENAME を MAX 以下の長さに短縮する.
パスの区切り文字は SEPARATOR, 短縮時の省略表示を OMIT で指定する."
  (let* ((max (or max (window-width)))
         (separator (or separator "/"))
         (omit (or omit "..."))
         (parts (split-string (abbreviate-file-name path) separator))
         (length (length (abbreviate-file-name path)))
         (target (if (string-empty-p (car parts)) 3 2))
         (min-parts (+ target 3)))
    (when (and (> length max)
               (> (length parts) min-parts))
      (setq length (+ length (length omit) (- (length (nth target parts)))))
      (setf (nth target parts) omit)
      (setq target (+ target 1))
      (while (and (> length max)
                  (> (length parts) min-parts))
        (setq length (- length (length (nth target parts)) 1))
        (setq parts (e:remove-nth target parts))))
    (string-join parts separator)))

(defun e:major-mode (buffer)
  (with-current-buffer buffer
    major-mode))
