
(defvar helm-insert-git-log-source
  (helm-build-in-buffer-source "Git log"
    :data #'helm-insert-git-log-source-data
    :real-to-display #'helm-insert-git-log-source-real-to-display
    :action #'helm-insert-git-log-source-action))

(defun helm-insert-git-log-regexp ()
  "\\(.+\\)\x0000\\(.+\\)")

(defun helm-insert-git-log-source-data ()
  (s-split "\n" (shell-command-to-string "git log --pretty=format:'%H%x00%s' --no-merges")))

(defun helm-insert-git-log-source-real-to-display (candidate)
  (let ((regexp (helm-insert-git-log-regexp)))
    (when (string-match regexp candidate)
      (format "%s %s"
              (propertize (match-string 1 candidate) 'face 'font-lock-comment-face)
              (match-string 2 candidate)))))

(defun helm-insert-git-log-source-action (candidate)
  (let ((regexp (helm-insert-git-log-regexp)))
    (when (string-match regexp candidate)
      (insert (match-string 2 candidate) "\n"))))

(defun helm-insert-git-log ()
  (interactive)
  (helm :sources helm-insert-git-log-source
        :buffer "*HELM Insert Git log*"))

(provide 'helm-insert-git-log)
