;;; funcs.el

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

(defun e:toggle-indent-tabs-mode ()
  "インデントモードをタブ/空白で切替える."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode: %s" indent-tabs-mode))
