
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
  (declare
   (type (integer 0) n)
   (type list list))
  (if (or (zerop n) (null list))
      (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))
