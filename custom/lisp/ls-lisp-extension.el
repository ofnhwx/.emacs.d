
(eval-and-compile
  (require 'ls-lisp)
  (require 'f)
  (require 's))



(defgroup ls-lisp-extension ()
  "`ls-lisp' をちょっと改造"
  :group 'ls-lisp)

(defface e:dired-directory-face
  '((t :foreground "#4f97d7"))
  "ディレクトリ"
  :group 'ls-lisp-extension)

(defface e:dired-symlink-face
  '((t :foreground "#28def0"))
  "シンボリックリンク"
  :group 'ls-lisp-extension)

(defface e:dired-hidden-file-face
  '((t :foreground "#707070"))
  "隠しファイル"
  :group 'ls-lisp-extension)

(defface e:dired-uid-face
  '((t :foreground "#90ee90"))
  "ユーザーID"
  :group 'ls-lisp-extension)

(defface e:dired-gid-face
  '((t :foreground "#90ee90"))
  "グループID"
  :group 'ls-lisp-extension)

(defface e:dired-modes-d-face
  '((t :foreground "#4f97d7"))
  "モード: d(ディレクトリ)"
  :group 'ls-lisp-extension)

(defface e:dired-modes-l-face
  '((t :foreground "#28def0"))
  "モード: l(リンク)"
  :group 'ls-lisp-extension)

(defface e:dired-modes-r-face
  '((t :foreground "#eeee00"))
  "モード: r(読込可)"
  :group 'ls-lisp-extension)

(defface e:dired-modes-w-face
  '((t :foreground "#ed2200"))
  "モード: w(書込可)"
  :group 'ls-lisp-extension)

(defface e:dired-modes-x-face
  '((t :foreground "#00ee00"))
  "モード: x(実行可)"
  :group 'ls-lisp-extension)

(defface e:dired-modes-s-face
  '((t :foreground "#9f79ee"))
  "モード: s(setuid/setgid + 実行可)"
  :group 'ls-lisp-extension)

(defface e:dired-modes-S-face
  '((t :foreground "#9f79ee"))
  "モード: S(setuid/setgid)"
  :group 'ls-lisp-extension)

(defface e:dired-modes-t-face
  '((t :foreground "#9f79ee"))
  "モード: t(sticky + 実行可)"
  :group 'ls-lisp-extension)

(defface e:dired-modes-T-face
  '((t :foreground "#9f79ee"))
  "モード: T(sticky)"
  :group 'ls-lisp-extension)

(defface e:dired-modes---face
  '((t :foreground "#707070"))
  "モード: -(その他)"
  :group 'ls-lisp-extension)

(defface e:dired-size-over-1t-face
  '((t :foreground "#ff7f00" :underline t))
  "1TiB以上のファイルサイズ"
  :group 'ls-lisp-extension)

(defface e:dired-size-over-1g-face
  '((t :foreground "#ff7f00"))
  "1GiB以上のファイルサイズ"
  :group 'ls-lisp-extension)

(defface e:dired-size-over-1m-face
  '((t :foreground "#ee7600"))
  "1MiB以上のファイルサイズ"
  :group 'ls-lisp-extension)

(defface e:dired-size-over-1k-face
  '((t :foreground "#cd6600"))
  "1KiB以上のファイルサイズ"
  :group 'ls-lisp-extension)

(defface e:dired-size-default-face
  '((t :foreground "#8b4500"))
  "1KiB未満のファイルサイズ"
  :group 'ls-lisp-extension)

(defface e:dired-time-over-1y-face
  '((t :foreground "#00688b"))
  "1年以上経過したファイル日時"
  :group 'ls-lisp-extension)

(defface e:dired-time-over-1m-face
  '((t :foreground "#009acd"))
  "1ヶ月以上経過したファイル日時"
  :group 'ls-lisp-extension)

(defface e:dired-time-over-1w-face
  '((t :foreground "#00b2ee"))
  "1週間以上経過したファイル日時"
  :group 'ls-lisp-extension)

(defface e:dired-time-over-1d-face
  '((t :foreground "#00bfff"))
  "1日以上経過したファイル日時"
  :group 'ls-lisp-extension)

(defface e:dired-time-default-face
  '((t :foreground "#00bfff" :underline t))
  "1日以内のファイル日時"
  :group 'ls-lisp-extension)



(define-advice ls-lisp-format (:filter-args (args) ex)
  (let* ((file-name  (nth 0 args))
         (file-attr  (nth 1 args))
         (file-size  (nth 2 args))
         (switches   (nth 3 args))
         (time-index (nth 4 args))
         (file-type  (nth 0 file-attr))
         (file-uid   (nth 2 file-attr))
         (file-gid   (nth 3 file-attr))
         (file-modes (nth 8 file-attr)))
    (cond
     ((or (s-starts-with? "." file-name)
          (s-starts-with? "#" file-name)
          (s-starts-with? "~" file-name))
      (setq file-name (propertize file-name 'font-lock-face 'e:dired-hidden-file-face)))
     ((eq file-type t)
      (setq file-name (propertize file-name 'font-lock-face 'e:dired-directory-face)))
     ((stringp file-type)
      (setq file-name (propertize file-name 'font-lock-face 'e:dired-symlink-face)))
     ((s-contains? "x" file-modes)
      (setq file-name (propertize file-name 'font-lock-face 'e:dired-modes-x-face))))
    (if (stringp file-type)
        (setf (nth 0 file-attr) (propertize (f-short file-type) 'font-lock-face 'e:dired-symlink-face)))
    (setf (nth 2 file-attr) (propertize file-uid 'font-lock-face 'e:dired-uid-face))
    (setf (nth 3 file-attr) (propertize file-gid 'font-lock-face 'e:dired-gid-face))
    (setf (nth 8 file-attr) (mapconcat (lambda (c)
                                         (case c
                                           (?d (propertize "d" 'font-lock-face 'e:dired-modes-d-face))
                                           (?l (propertize "l" 'font-lock-face 'e:dired-modes-l-face))
                                           (?r (propertize "r" 'font-lock-face 'e:dired-modes-r-face))
                                           (?w (propertize "w" 'font-lock-face 'e:dired-modes-w-face))
                                           (?x (propertize "x" 'font-lock-face 'e:dired-modes-x-face))
                                           (?s (propertize "s" 'font-lock-face 'e:dired-modes-s-face))
                                           (?S (propertize "S" 'font-lock-face 'e:dired-modes-S-face))
                                           (?t (propertize "t" 'font-lock-face 'e:dired-modes-t-face))
                                           (?T (propertize "T" 'font-lock-face 'e:dired-modes-T-face))
                                           (?- (propertize "-" 'font-lock-face 'e:dired-modes---face))
                                           (t  (propertize (string c) 'font-lock-face 'e:dired-modes---face))))
                                       file-modes ""))
    (list file-name file-attr file-size switches time-index)))

(define-advice ls-lisp-format-time (:around (fn file-attr time-index) ex)
  (let* ((time (nth (or time-index 5) file-attr))
         (diff (time-subtract time nil))
         (formated-time (funcall fn file-attr time-index)))
    (cond
     ((time-less-p diff -31536000)
      (propertize formated-time 'font-lock-face 'e:dired-time-over-1y-face))
     ((time-less-p diff -2592000)
      (propertize formated-time 'font-lock-face 'e:dired-time-over-1m-face))
     ((time-less-p diff -604800)
      (propertize formated-time 'font-lock-face 'e:dired-time-over-1w-face))
     ((time-less-p diff -86400)
      (propertize formated-time 'font-lock-face 'e:dired-time-over-1d-face))
     (t
      (propertize formated-time 'font-lock-face 'e:dired-time-default-face)))))

(define-advice ls-lisp-format-file-size (:around (fn file-size human-readable) ex)
  (let ((formated-file-size (funcall fn file-size human-readable)))
    (cond
     ((>= file-size 1099511627776)
      (propertize formated-file-size 'font-lock-face 'e:dired-size-over-1t-face))
     ((>= file-size 1073741824)
      (propertize formated-file-size 'font-lock-face 'e:dired-size-over-1g-face))
     ((>= file-size 1048576)
      (propertize formated-file-size 'font-lock-face 'e:dired-size-over-1m-face))
     ((>= file-size 1024)
      (propertize formated-file-size 'font-lock-face 'e:dired-size-over-1k-face))
     (t
      (propertize formated-file-size 'font-lock-face 'e:dired-size-default-face)))))



(provide 'ls-lisp-extension)
