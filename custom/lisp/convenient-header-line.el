
(eval-and-compile
  (require 'convenient-features))



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



(defvar e:header-line-for-files-cache (make-hash-table :test 'equal)
  "ファイル用のヘッダーラインのフォーマットのキャッシュ.")

(defun e:header-line-for-files-format--internal (path &optional refname)
  "ファイル用のヘッダーラインのフォーマットを作成する内部処理"
  (let* ((separator-left  #'powerline-wave-right)
         (separator-right #'powerline-wave-left)
         (tramp (file-remote-p path))
         (proot (e:project-root path))
         (pname (and proot (e:project-name proot)))
         (file  (f-filename path))
         (dir   (f-short (file-name-directory path)))
         (lhs nil)
         (rhs nil))
    (when proot
      (setq dir (s-replace proot "" dir))
      (setq lhs (-snoc lhs
                       (powerline-raw (concat pname " ") 'powerline-active1 'l)
                       (funcall separator-left 'powerline-active1 'powerline-inactive1))))
    (cond
     (tramp
      (setq dir (s-replace tramp "" dir))
      (setq rhs (-snoc rhs
                       (funcall separator-right 'powerline-inactive1 'powerline-active1)
                       (powerline-raw (concat " " (string-trim-right tramp ":")) 'powerline-active1 'r))))
     (refname
      (setq rhs (-snoc rhs
                       (funcall separator-right 'powerline-inactive1 'powerline-active1)
                       (powerline-raw (concat " " refname) 'powerline-active1 'r)))))
    (setq lhs (-snoc lhs
                     (powerline-raw dir 'mode-line 'l)
                     (powerline-raw file 'font-lock-keyword-face)))
    (concat (powerline-render lhs)
            (powerline-fill 'mode-line (powerline-width rhs))
            (powerline-render rhs))))

(defun e:header-line-for-files-format ()
  "ファイル用のヘッダーラインのフォーマット."
  (let* ((path    (or (f-long (e:current-buffer-file-name)) ""))
         (refname (e:current-buffer-refname))
         (key     (concat path (or refname ""))))
    (or (gethash key e:header-line-for-files-cache)
        (puthash key (e:header-line-for-files-format--internal path refname)
                 e:header-line-for-files-cache))))

(defun e:setup-header-line-for-files ()
  "ファイル用のヘッダーラインの設定."
  (set-face-attribute 'header-line nil :inherit 'mode-line)
  (setq-local spaceline-buffer-id-p nil)
  (setq-local header-line-format
              '((:eval (e:header-line-for-files-format)))))



(provide 'convenient-header-line)
