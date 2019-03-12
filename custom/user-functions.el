
(progn ;; フォント設定
  (defvar e:font-name "Ricty Diminished Discord")
  (defvar e:font-size 14)
  (defvar e:font-rescale 1.00)
  (defun e:font ()
    "フォント"
    (list e:font-name :size e:font-size)))

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

(defun e:shorten (filename &optional max)
  "指定された FILENAME を MAX 以下の長さに短縮する."
  (let ((filename (abbreviate-file-name filename))
        (max (or max (window-width)))
        (target 3)
        (break nil))
    (while (and (> (length filename) max)
                (not break))
      (let ((parts (split-string filename "/")))
        (if (> (length parts) (+ target 2))
            (progn
              (if (equal (nth target parts) "...")
                  (setq parts (remove (nth (+ target 1) parts) parts))
                (setf (nth target parts) "..."))
              (setq filename (string-join parts "/")))
          (setq break t))))
    filename))
