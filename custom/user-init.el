
;; ファイルを移動
(progn
  (set-variable 'custom-file (expand-file-name "custom.el" e:private-directory))
  (set-variable 'auth-sources `(,(expand-file-name "authinfo.plist" e:private-directory)))
  (set-variable 'spacemacs-env-vars-file (expand-file-name "spacemacs.env" e:private-directory))
  (set-variable 'url-cache-directory (expand-file-name "url/cache" e:private-directory))
  (set-variable 'url-cookie-file (expand-file-name "url/cookies" e:private-directory)))

;; shellの設定
(set-variable 'shell-file-name
              (or (executable-find "zsh")
                  (executable-find "bash")
                  (executable-find "sh")))

;; パスワード関連
(progn
  (set-variable 'password-cache-expiry 3600)
  (set-variable 'plstore-encoded t))

;; 折り返さない
(progn
  (setq-default truncate-lines t)
  (set-variable 'truncate-partial-width-windows nil))

;; 最終行の改行は`EditorConfig'で管理
(progn
  (set-variable 'mode-require-final-newline nil)
  (set-variable 'require-final-newline nil))

;; ロックファイルを作成しない
(set-variable 'create-lockfiles nil)

;; 右から左に読む言語に対応しない
(setq-default bidi-display-reordering nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spacemacs|use-package-add-hook eshell
  :post-init
  (set-variable 'eshell-directory-name (expand-file-name "eshell" e:private-directory)))

(spacemacs|use-package-add-hook ddskk
  :post-init
  (set-variable 'default-input-method "japanese-skk")
  (progn
    (set-variable 'skk-user-directory (expand-file-name "ddskk" e:private-directory))
    (set-variable 'skk-large-jisyo (expand-file-name "dic-mirror/SKK-JISYO.L" e:util-directory)))
  (progn
    (set-variable 'skk-preload t)
    (set-variable 'skk-egg-like-newline t)
    (set-variable 'skk-share-private-jisyo t)
    (set-variable 'skk-show-annotation t)
    (set-variable 'skk-show-inline 'vertical)
    (set-variable 'skk-sticky-key ";")
    (set-variable 'skk-use-jisx0201-input-method t))
  (when (executable-find "google-ime-skk")
    (set-variable 'skk-server-prog (executable-find "google-ime-skk"))
    (set-variable 'skk-server-inhibit-startup-server t)
    (set-variable 'skk-server-host "127.0.0.1")
    (set-variable 'skk-server-portnum 55100)))

(spacemacs|use-package-add-hook google-translate
  :post-init
  (set-variable 'google-translate-default-source-language nil)
  (set-variable 'google-translate-default-target-language "ja"))

(spacemacs|use-package-add-hook recentf
  :post-init
  (set-variable 'recentf-max-menu-items 20)
  (set-variable 'recentf-max-saved-items 3000)
  (set-variable 'recentf-filename-handlers '(abbreviate-file-name))
  :post-config
  (progn
    (defun e:recentf-save-list:before (&rest args)
      (let ((list nil))
        (dolist (file (mapcar 'abbreviate-file-name recentf-list))
          (or (member file list)
              (push file list)))
        (setq recentf-list (reverse list))))
    (advice-add 'recentf-save-list :before 'e:recentf-save-list:before)))
