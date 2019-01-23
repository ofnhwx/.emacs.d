
;; ファイルを移動
(setq custom-file (expand-file-name "custom.el" e:custom-directory))
(setq eshell-directory-name (expand-file-name "eshell" spacemacs-cache-directory))
(setq url-cache-directory (expand-file-name "url/cache" spacemacs-cache-directory))
(setq url-cookie-file (expand-file-name "url/cookies"))

;; キーバインドの調整
(setq evil-toggle-key "C-z z")
(setq eyebrowse-keymap-prefix (kbd "C-z"))

;; SKKを起動時に読込み
(setq skk-preload t)

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
