;;; funcs.el

(defun e:auth-source-get (property &rest spec)
  "認証情報から SPEC に一致する項目の PROPERTY を取得する."
  (let ((plist (car (apply 'auth-source-search spec)))
        (pkey (intern (format ":%s" property))))
    (when plist
      (plist-get plist pkey))))

(defun browse-url-by-choosen (url &optional new-window)
  "選択したブラウザで URL を開く."
  (interactive)
  (let ((browser (completing-read "Choose Browser:"
                                  '(eww-browse-url browse-url-default-browser))))
    (funcall (intern browser) url new-window)))

(defun e:toggle-indent-tabs-mode ()
  "インデントモードをタブ/空白で切替える."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode: %s" indent-tabs-mode))
