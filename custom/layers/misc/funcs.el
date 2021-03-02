;;; funcs.el

(defun browse-url-by-choosen (url &optional new-window)
  "選択したブラウザで URL を開く."
  (interactive)
  (let ((browsers '(eww-browse-url browse-url-default-browser)))
    (when browse-url-generic-program
      (add-to-list 'browsers 'browse-url-generic t))
    (funcall (intern (completing-read "Choose Browser: " browsers)) url new-window)))

(defun e:toggle-indent-tabs-mode ()
  "インデントモードをタブ/空白で切替える."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode: %s" indent-tabs-mode))
