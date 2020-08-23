
(eval-and-compile
  (require 'e:convenient-features))



(defun e:setup-header-line ()
  "ヘッダーラインをなるべくいい感じに設定する"
  (cl-loop for buffer in (--filter (not (buffer-local-value 'header-line-format it))
                                   (-map #'window-buffer (window-list)))
           do (with-current-buffer buffer
                (cond
                 ((e:current-buffer-file-name)
                  (e:setup-header-line-for-files))))))

(defun e:convenient-header-line-start ()
  "自分にとって便利なヘッダーラインの表示を開始する."
  (interactive)
  (run-with-idle-timer 1.0 1.0 'e:setup-header-line))



(defun e:header-line-for-files-format ()
  "ファイル用のヘッダーラインのフォーマット."
  (let ((path (f-short (e:current-buffer-file-name)))
        (cache '(:path nil :project-root nil :project-name nil)))
    (unless (and (local-variable-p 'convenient-header-line--cache)
                 (s-equals? path (plist-get 'convenient-header-line--cache :path)))
      (plist-put cache :path path)
      (plist-put cache :project-root (e:project-root path))
      (plist-put cache :project-name (and (plist-get cache :project-root)
                                          (e:project-name (plist-get cache :project-root))))
      (setq-local convenient-header-line--cache cache)))
  (let* ((separator-l #'powerline-wave-right)
         (separator-r #'powerline-wave-left)
         (active (powerline-selected-window-active))
         (face0 (if active 'powerline-active0 'powerline-inactive0))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (file   (file-name-nondirectory (plist-get convenient-header-line--cache :path)))
         (dir    (file-name-directory    (plist-get convenient-header-line--cache :path)))
         (remote (file-remote-p          (plist-get convenient-header-line--cache :path)))
         (project-root (plist-get convenient-header-line--cache :project-root))
         (project-name (plist-get convenient-header-line--cache :project-name))
         (refname (e:current-buffer-refname))
         lhs rhs)
    ;; Left
    (when project-root
      (setq dir (s-replace project-root "" dir))
      (setq lhs (-snoc lhs (powerline-raw (concat project-name " ") face2 'l))))
    (when lhs
      (setq lhs (-snoc lhs (funcall separator-l face2 face0))))
    ;; Right
    (cond
     (remote
      (setq dir (s-replace remote "" dir))
      (setq rhs (-snoc rhs (powerline-raw (concat " " (string-trim-right remote ":")) face2 'r))))
     (refname
      (setq rhs (-snoc rhs (powerline-raw (concat " " refname) face2 'r)))))
    (when rhs
      (push (funcall separator-r face0 face2) rhs))
    ;; Center
    (setq lhs (-snoc lhs
                     (powerline-raw dir face0 'l)
                     (powerline-raw file 'font-lock-keyword-face)))
    ;;
    (concat (powerline-render lhs)
            (powerline-fill 'mode-line (powerline-width rhs))
            (powerline-render rhs))))

(defun e:setup-header-line-for-files ()
  "ファイル用のヘッダーラインの設定."
  (set-face-attribute 'header-line nil :inherit 'mode-line)
  (setq-local spaceline-buffer-id-p nil)
  (setq-local header-line-format
              '((:eval (e:header-line-for-files-format)))))



(provide 'e:convenient-header-line)
