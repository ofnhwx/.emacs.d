;;; funcs.el

(defun browse-url-by-choosen (url &optional new-window)
  "選択したブラウザで URL を開く."
  (interactive)
  (let ((browsers '(eww-browse-url browse-url-default-browser)))
    (when browse-url-generic-program
      (add-to-list 'browsers 'browse-url-generic t))
    (funcall (intern (completing-read "Choose Browser: " browsers)) url new-window)))

(spacemacs|add-toggle indent-tabs-mode
  :status indent-tabs-mode
  :on (setq indent-tabs-mode t)
  :off (setq indent-tabs-mode nil)
  :evil-leader "tT")
