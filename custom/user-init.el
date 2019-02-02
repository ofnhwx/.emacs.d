
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

(spacemacs|use-package-add-hook ace-window
  :post-init
  (bind-keys
   :map global-map
   ("C-^" . ace-window)))

(spacemacs|use-package-add-hook eshell
  :post-init
  (set-variable 'eshell-directory-name (expand-file-name "eshell" e:private-directory)))

(spacemacs|use-package-add-hook ddskk
  :post-init
  (set-variable 'default-input-method "japanese-skk")
  (progn
    (set-variable 'skk-user-directory (expand-file-name "ddskk" e:private-directory))
    (set-variable 'skk-large-jisyo (expand-file-name "dic-mirror/SKK-JISYO.L" e:external-directory)))
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

(spacemacs|use-package-add-hook flycheck
  :post-init
  (set-variable 'flycheck-idle-buffer-switch-delay 3.0)
  (set-variable 'flycheck-idle-change-delay 3.0))

(spacemacs|use-package-add-hook google-translate
  :post-init
  (set-variable 'google-translate-default-source-language nil)
  (set-variable 'google-translate-default-target-language "ja"))

(spacemacs|use-package-add-hook helm
  :post-init
  (bind-key [remap eval-expression] 'helm-eval-expression)
  (with-eval-after-load 'eldoc-eval
    (bind-key [remap eldoc-eval-expression] 'helm-eval-expression eldoc-in-minibuffer-mode-map)))

(spacemacs|use-package-add-hook lsp
  :post-config
  (let ((cmd (expand-file-name "lsp/php/vendor/felixfbecker/language-server/bin/php-language-server.php" e:private-directory)))
    (setq lsp-clients-php-server-command `("php" ,cmd))))

(spacemacs|use-package-add-hook magit
  :post-init
  (set-variable 'magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 15))
  (set-variable 'magit-diff-refine-hunk 'all)
  (set-variable 'smerge-refine-ignore-whitespace nil)
  :post-config
  (magit-define-popup-switch 'magit-log-popup ?l "Always sort by date" "--date-order"))

(spacemacs|use-package-add-hook notmuch
  :post-init
  (set-variable 'notmuch-archive-tags '("-inbox" "-unread"))
  (set-variable 'notmuch-column-control 1.0)
  (set-variable 'notmuch-hello-thousands-separator ",")
  (set-variable 'notmuch-search-oldest-first nil)
  (set-variable 'notmuch-show-empty-saved-searches t)
  (set-variable 'notmuch-show-logo nil)
  (set-variable 'notmuch-hello-hide-tags
                '("encrypted" "drafts" "flagged" "inbox" "sent" "signed" "spam" "unread"))
  (set-variable 'notmuch-saved-searches
                '((:name "受信トレイ" :query "tag:inbox"   :key "i")
                  (:name "未読　　　" :query "tag:unread"  :key "u")
                  (:name "スター付き" :query "tag:flagged" :key "f")
                  (:name "送信済み　" :query "tag:sent"    :key "t")
                  (:name "下書き　　" :query "tag:draft"   :key "d")
                  (:name "すべて　　" :query "*"           :key "a")
                  (:name "迷惑メール" :query "tag:spam"    :key "s")))
  (setenv "XAPIAN_CJK_NGRAM" "1"))

(spacemacs|use-package-add-hook org
  :post-init
  (set-variable 'org-directory (expand-file-name "org" e:private-directory))
  (let ((org-agenda-directory (expand-file-name "agenda" org-directory)))
    (when (file-directory-p org-agenda-directory)
      (set-variable 'org-agenda-files (cl-remove-if 'file-directory-p (directory-files org-agenda-directory t))))))

(spacemacs|use-package-add-hook pangu-spacing
  :post-init
  (set-variable 'pangu-spacing-real-insert-separtor nil))

(spacemacs|use-package-add-hook prodigy
  :post-init
  (defun e:prodigy-start-service (name)
    (let ((service (prodigy-find-service name)))
      (when service
        (prodigy-start-service service)))))

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
