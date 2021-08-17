;;; custom-headerline.el

(eval-and-compile
  (require 'dash))



(defun custom-headerline-start ()
  (interactive)
  (add-hook 'window-buffer-change-functions 'chl--setup))



(defun chl--setup-1 (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((chl--filename)
      (setq-local spaceline-buffer-id-p nil)
      (setq-local header-line-format '((:eval (chl--file-format)))))
     ((derived-mode-p 'magit-status-mode 'magit-process-mode 'messages-buffer-mode)
      (setq-local spaceline-buffer-id-p nil)
      (setq-local header-line-format '((:eval (buffer-name))))))))

(defun chl--setup (&optional frame)
  (set-face-attribute 'header-line nil :inherit 'mode-line)
  (--each (->> (window-list frame)
               (-map 'window-buffer)
               (--remove (buffer-local-value 'header-line-format it)))
    (chl--setup-1 it)))

(defun chl--file-format ()
  (unless (chl--cache-exists)
    (chl--make-cache))
  (let* ((separator-l (intern (format "powerline-%s-left"  (spacemacs/mode-line-separator))))
         (separator-r (intern (format "powerline-%s-right" (spacemacs/mode-line-separator))))
         (face0 (if (powerline-selected-window-active) 'powerline-active0 'powerline-inactive0))
         (face1 (if (powerline-selected-window-active) 'powerline-active1 'powerline-inactive1))
         (face2 (if (powerline-selected-window-active) 'powerline-active2 'powerline-inactive2))
         (file   (file-name-nondirectory (chl--get-cache-path)))
         (dir    (file-name-directory    (chl--get-cache-path)))
         (project-root (chl--get-cache-proot))
         (project-name (chl--get-cache-pname))
         (refname (chl--refname))
         (lhs nil)
         (rhs nil))
    ;; 余計な部分を削る
    (when project-root
      (setq dir (s-replace project-root "" dir)))
    ;; 左側
    (when project-name
      (setq lhs (-snoc lhs (powerline-raw (concat project-name " ") face0 'l)))
      (setq lhs (-snoc lhs (funcall separator-l face0 face1))))
    ;; 中央
    (let ((foreground "#44bc44")
          (background (face-attribute face1 :background)))
      (setq file (propertize file 'face (list :foreground foreground :background background))))
    (setq lhs (-snoc lhs (powerline-raw dir face1 'l)))
    (setq lhs (-snoc lhs (powerline-raw file nil 'r)))
    ;; 右側
    (when refname
      (setq rhs (-snoc rhs (funcall separator-r face1 face0)))
      (setq rhs (-snoc rhs (powerline-raw (concat " " refname) face0 'r))))
    ;; 最後に合わせる
    (concat (powerline-render lhs)
            (powerline-fill face2 (powerline-width rhs))
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
      (when (bound-and-true-p gist-id)
        (plist-put cache :path  (s-replace "#" "/" path))
        (plist-put cache :proot nil)
        (plist-put cache :pname (format "Gist:%s" (oref (gist-list-db-get-gist gist-id) :description))))
      (with-no-warnings (setq-local chl--cache cache)))))

(progn ;; 情報取得
  (defun chl--filename ()
    (let ((filename (or buffer-file-name
                        (bound-and-true-p magit-buffer-file-name)
                        (bound-and-true-p gist-filename))))
      (and filename
           (f-short filename))))
  (defun chl--refname ()
    (or (and vc-mode (s-trim (kllib:unpropertize vc-mode)))
        (bound-and-true-p magit-buffer-refname)
        (bound-and-true-p gist-id))))



(provide 'custom-headerline)

;;; custom-headerline.el ends here
