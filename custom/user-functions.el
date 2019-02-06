
(defun e:load-custom-file (filename)
  "FILENAME で指定した設定ファイルを読込む."
  (let ((filename (if (file-name-absolute-p filename)
                      filename
                    (expand-file-name filename e:custom-directory)))
        (extension (file-name-extension filename)))
    (if (equalp extension "org")
        (progn (require 'org-install)
               (org-babel-load-file filename))
      (load-file filename))))

(defun e:setup-font ()
  "フォント等の設定"
  (interactive)
  ;; エンコーディング設定
  (set-language-environment "Japanese")
  (let ((coding-system 'utf-8))
    (prefer-coding-system          coding-system)
    (set-default-coding-systems    coding-system)
    (set-buffer-file-coding-system coding-system)
    (set-terminal-coding-system    coding-system)
    (set-keyboard-coding-system    coding-system))
  ;; フォント設定
  (let* ((charsets '(japanese-jisx0208
                     japanese-jisx0208-1978
                     japanese-jisx0212
                     japanese-jisx0213-1
                     japanese-jisx0213-2
                     japanese-jisx0213.2004-1
                     japanese-jisx0213-a
                     katakana-jisx0201
                     katakana-sjis))
         (fontspec (font-spec :family e:font-name)))
    (setq face-font-rescale-alist `((,e:font-name . ,e:font-rescale)))
    (set-face-attribute 'default nil :family e:font-name :height e:font-height)
    (dolist (charset charsets)
      (set-fontset-font t charset fontspec)))
  ;; 対策: East Asian Ambiguous Width
  (add-to-list 'load-path (expand-file-name "locale-eaw" e:external-directory))
  (when (require 'eaw nil t)
    (eaw-fullwidth))
  ;;
  t)

(defun e:auth-source-get (property &rest spec)
  "認証情報から SPEC に一致する項目の PROPERTY を取得する."
  (let ((plist (car (apply 'auth-source-search spec)))
        (pkey (intern (format ":%s" property))))
    (when plist
      (plist-get plist pkey))))

(defun e:system-type-darwin-p ()
  "OS が Mac かを取得する."
  (eq system-type 'darwin))

(defun e:remove-nth (n list)
  "N 番目の要素を LIST から取り除いて返す."
  (if (or (zerop n) (null list))
      (cdr list)
    (cons (car list) (e:remove-nth (1- n) (cdr list)))))
