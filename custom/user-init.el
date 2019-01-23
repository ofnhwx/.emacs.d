
;; ファイルを移動
(setq custom-file (expand-file-name "custom.el" e:custom-directory))
(setq eshell-directory-name (expand-file-name "eshell" spacemacs-cache-directory))
(setq url-cache-directory (expand-file-name "url/cache" spacemacs-cache-directory))
(setq url-cookie-file (expand-file-name "url/cookies"))

;; キーバインドの調整
(setq evil-toggle-key "C-z z")
(setq eyebrowse-keymap-prefix (kbd "C-z"))

;; SKK
(progn
  (set-variable 'default-input-method "japanese-skk")
  ;; パス
  (set-variable 'skk-user-directory (expand-file-name "ddskk" e:util-directory))
  (set-variable 'skk-large-jisyo (expand-file-name "SKK-JISYO.L" skk-user-directory))
  ;; 各種設定
  (set-variable 'skk-preload t)
  (set-variable 'skk-egg-like-newline t)
  (set-variable 'skk-share-private-jisyo t)
  (set-variable 'skk-show-annotation t)
  (set-variable 'skk-show-inline 'vertical)
  (set-variable 'skk-sticky-key ";")
  (set-variable 'skk-use-jisx0201-input-method t)
  ;; skk-server
  (when (executable-find "google-ime-skk")
    (set-variable 'skk-server-prog (executable-find "google-ime-skk"))
    (set-variable 'skk-server-inhibit-startup-server t)
    (set-variable 'skk-server-host "127.0.0.1")
    (set-variable 'skk-server-portnum 55100)))

;; 折り返さない
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; 最終行の改行は`EditorConfig'で管理
(setq mode-require-final-newline nil)
(setq require-final-newline nil)

;; ロックファイルを作成しない
(setq create-lockfiles nil)

;; 右から左に読む言語に対応しない
(setq-default bidi-display-reordering nil)
