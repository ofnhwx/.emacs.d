;;; funcs.el

(defun affe-find-in-project (&optional initial)
  (interactive)
  (let ((project-root (kllib:project-root default-directory)))
    (affe-find project-root initial)))
