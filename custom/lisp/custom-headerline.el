;;; custom-headerline.el



(defun custom-headerline-start ()
  (interactive)
  (run-with-idle-timer 1.0 1.0 #'chl--setup))



(defun chl--setup ()
  (let ((buffers (--remove (buffer-local-value 'header-line-format it)
                           (-map #'window-buffer (window-list)))))
    (-each buffers
      (lambda (buffer)
        (with-current-buffer buffer
          (cond
           ((chl--filename)
            (setq-local header-line-format '((:eval (chl--file-format))))))))))
  ;; ここはちょっと無理矢理
  (set-face-attribute 'header-line nil :inherit 'mode-line)
  (setq-local spaceline-buffer-id-p nil))

(defun chl--file-format ()
  (unless (chl--cache-exists)
    (chl--make-cache))
  (let* ((separator-l #'powerline-wave-right)
         (separator-r #'powerline-wave-left)
         (face0 (if (powerline-selected-window-active) 'powerline-active0 'powerline-inactive0))
         (face1 (if (powerline-selected-window-active) 'powerline-active1 'powerline-inactive1))
         (face2 (if (powerline-selected-window-active) 'powerline-active2 'powerline-inactive2))
         (file   (file-name-nondirectory (chl--get-cache-path)))
         (dir    (file-name-directory    (chl--get-cache-path)))
         (remote (file-remote-p          (chl--get-cache-path)))
         (project-root (chl--get-cache-proot))
         (project-name (chl--get-cache-pname))
         (refname (chl--refname))
         (lhs nil)
         (rhs nil))
    ;; 左側
    (when project-root
      (setq dir (s-replace project-root "" dir))
      (setq lhs (-snoc lhs (powerline-raw (concat project-name " ") face2 'l))))
    (when lhs
      (setq lhs (-snoc lhs (funcall separator-l face2 face0))))
    ;; 右側
    (cond
     (remote
      (setq dir (s-replace remote "" dir))
      (setq rhs (-snoc rhs (powerline-raw (concat " " (string-trim-right remote ":")) face2 'r))))
     (refname
      (setq rhs (-snoc rhs (powerline-raw (concat " " refname) face2 'r)))))
    (when rhs
      (push (funcall separator-r face0 face2) rhs))
    ;; 中央
    (setq lhs (-snoc lhs
                     (powerline-raw dir face0 'l)
                     (powerline-raw file 'font-lock-keyword-face)))
    ;; 最後に合わせる
    (concat (powerline-render lhs)
            (powerline-fill 'mode-line (powerline-width rhs))
            (powerline-render rhs))))



(progn ;; キャッシュ関連
  (defun chl--get-cache-path ()
    (plist-get (bound-and-true-p chl--cache) :path))
  (defun chl--get-cache-proot ()
    (plist-get (bound-and-true-p chl--cache) :proot))
  (defun chl--get-cache-pname ()
    (plist-get (bound-and-true-p chl--cache) :pname))
  (defun chl--cache-exists ()
    (and (local-variable-p 'chl--cache)
         (s-equals? (chl--get-cache-path) (chl--filename))))
  (defun chl--make-cache ()
    (let* ((path  (chl--filename))
           (proot (kllib:project-root path))
           (pname (kllib:project-name proot))
           (cache (list :path path :proot proot :pname pname)))
      (with-no-warnings (setq-local chl--cache cache)))))

(progn ;; 情報取得
  (defun chl--filename ()
    (let ((filename (or buffer-file-name
                        (bound-and-true-p magit-buffer-file-name)
                        (bound-and-true-p gist-filename))))
      (and filename
           (f-short filename))))
  (defun chl--refname ()
    (let* ((path   (chl--filename))
           (remote (and path (file-remote-p path))))
      (or (and remote  (string-trim-right remote ":"))
          (and vc-mode (s-trim (kllib:unpropertize vc-mode)))
          (bound-and-true-p magit-buffer-refname)
          (bound-and-true-p gist-id)))))



(provide 'custom-headerline)

;;; custom-headerline.el ends here
