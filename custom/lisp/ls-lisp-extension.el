
(require 'ls-lisp)



(defface e:dired-time-over-1y-face
  '((t :foreground "#00688b"))
  "1年以上経過したファイル日時")

(defface e:dired-time-over-1m-face
  '((t :foreground "#009acd"))
  "1ヶ月以上経過したファイル日時")

(defface e:dired-time-over-1w-face
  '((t :foreground "#00b2ee"))
  "1週間以上経過したファイル日時")

(defface e:dired-time-over-1d-face
  '((t :foreground "#00bfff"))
  "1日以上経過したファイル日時")

(defface e:dired-time-default-face
  '((t :foreground "#00bfff" :underline t))
  "1日以内のファイル日時")

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



(defface e:dired-size-over-1t-face
  '((t :foreground "#ff7f00" :underline t))
  "1TiB以上のファイルサイズ")

(defface e:dired-size-over-1g-face
  '((t :foreground "#ff7f00"))
  "1GiB以上のファイルサイズ")

(defface e:dired-size-over-1m-face
  '((t :foreground "#ee7600"))
  "1MiB以上のファイルサイズ")

(defface e:dired-size-over-1k-face
  '((t :foreground "#cd6600"))
  "1KiB以上のファイルサイズ")

(defface e:dired-size-default-face
  '((t :foreground "#8b4500"))
  "1KiB未満のファイルサイズ")

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
