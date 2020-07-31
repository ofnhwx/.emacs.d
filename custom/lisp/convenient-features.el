
(eval-and-compile
  (require 'dash)
  (require 'f)
  (require 's))



(defvar e:project-root-mark '(".git/" ".hg/" ".svn/" ".git")
  "プロジェクトルートの判定に使用するファイル名の一覧.")

(defun e:project-root (filename)
  "指定した FILENAME のプロジェクトルートを返す."
  (let ((dirname (file-name-directory (f-expand filename))))
    (->> e:project-root-mark
         (--map (locate-dominating-file dirname it))
         (--max-by (> (length it) (length other))))))

(defun e:project-name (root)
  "指定した ROOT のプロジェクトの名前をいい感じに返す."
  (let* ((root (directory-file-name root))
         (name (file-name-nondirectory root))
         (parent (e:project-root root)))
    (if parent
        (s-concat (e:project-name parent) "/" name)
      name)))

(defun e:major-mode (&optional buffer)
  "指定した BUFFER のメジャーモードを取得する."
  (buffer-local-value 'major-mode (or buffer (current-buffer))))

(defun e:current-buffer-file-name ()
  "現在のバッファーの名前をいい感じに取得する"
  (or buffer-file-name
      (bound-and-true-p magit-buffer-file-name)))

(defun e:current-buffer-refname ()
  "現在のバッファーの refname をいい感じに取得する"
  (let* ((path   (e:current-buffer-file-name))
         (remote (ignore-errors (file-remote-p path))))
    (or (and remote (string-trim-right remote ":"))
        (and vc-mode (s-trim (e:unpropertize vc-mode)))
        (bound-and-true-p magit-buffer-refname))))



(defun e:shell-command-to-string (command)
  "コマンドの結果を文字列で取得."
  (s-trim (shell-command-to-string command)))

(defun e:shell-command-to-list (command)
  "コマンドの結果をリストで取得."
  (s-split "\n" (e:shell-command-to-string command)))



(defun e:unpropertize (text)
  "TEXT からテキストプロパティを除いた文字列を取得する."
  (let ((s (s-concat text)))
    (set-text-properties 0 (length s) nil s)
    s))

(defun e:auth-source-get (property &rest spec)
  "認証情報から SPEC に一致する項目の PROPERTY を取得する."
  (let ((plist (car (apply 'auth-source-search spec)))
        (pkey (intern (format ":%s" property))))
    (when plist
      (plist-get plist pkey))))

(defun e:remove-nth (n list)
  "N 番目の要素を LIST から取り除いて返す."
  (if (or (zerop n) (null list))
      (cdr list)
    (cons (car list) (e:remove-nth (1- n) (cdr list)))))

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

(defmacro e:place-in-cache (variable path)
  "VARIABLE のファイルを PATH に指定した名称でキャッシュディレクトリに設定する."
  `(set-variable ',variable (expand-file-name ,path spacemacs-cache-directory)))

(defmacro e:define-on/off-function (name)
  "NAME で指定されたマイナーモードをON/OFFする関数を作成する."
  `(progn
     (defun ,(intern (format "e:%s-on" name)) ()
       (interactive)
       (,(intern (format "%s" name)) 1))
     (defun ,(intern (format "e:%s-off" name)) ()
       (interactive)
       (,(intern (format "%s" name)) 0))))

(defun e:toggle-indent-tabs-mode ()
  "インデントモードをタブ/空白で切替える."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode: %s" indent-tabs-mode))

(defun e:default-pop-shell ()
  "ターミナルを開くやつ"
  (interactive)
  (let* ((key (read-event (format "open %s(0-9): " shell-default-shell) nil 0.8))
         (current-prefix-arg (if (member key (number-sequence ?0 ?9))
                                 (- key ?0)
                               current-prefix-arg))
         (current-buffer-name (buffer-name)))
    (call-interactively #'spacemacs/default-pop-shell)
    (message "%s → %s" current-buffer-name (buffer-name))))



(provide 'convenient-features)
