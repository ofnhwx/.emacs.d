;;; user-config.el

(eval-and-compile
  (require 'leaf)
  (require 'komunan-lisp-library)
  (require 'user-macro))



(leaf command-logger
  :config
  (command-logger-on))

(leaf custom-headerline
  :require t
  :config
  (custom-headerline-start))



(leaf japanese-language-environment
  :doc "言語環境を日本語に設定"
  :config
  (set-language-environment "Japanese")
  :doc "エンコーディングを設定"
  :config
  (let ((coding-system 'utf-8))
    (prefer-coding-system          coding-system)
    (set-default-coding-systems    coding-system)
    (set-buffer-file-coding-system coding-system)
    (set-terminal-coding-system    coding-system)
    (set-keyboard-coding-system    coding-system))
  :doc "ロケールを設定"
  :config
  (let ((value "ja_JP.UTF-8"))
    (setenv "LANG" value)
    (setenv "LC_ALL" value)))

(leaf appearance
  :doc "モードラインに不要な表示をしない"
  :config
  (spacemacs/defer-until-after-user-config #'spacemacs/toggle-mode-line-version-control-off)
  (set-variable 'spaceline-selection-info-p nil)
  :doc "モードラインのファイルエンコーディングの表示を改善"
  :config
  (spaceline-define-segment buffer-encoding-abbrev
    "The line ending convention used in the buffer."
    (let ((buf-coding (format "%s" buffer-file-coding-system)))
      (list (replace-regexp-in-string "-with-signature\\|-unix\\|-dos\\|-mac" "" buf-coding)
            (concat (and (string-match "with-signature" buf-coding) "ⓑ")
                    (and (string-match "unix"           buf-coding) "ⓤ")
                    (and (string-match "dos"            buf-coding) "ⓓ")
                    (and (string-match "mac"            buf-coding) "ⓜ")
                    )))
    :separator " "))

(leaf general
  :doc "エイリアス"
  :config
  (defalias 'exit 'save-buffers-kill-terminal)
  (defalias 'yes-or-no-p 'y-or-n-p)
  :doc "キーバインディング"
  :config
  (spacemacs/set-leader-keys
    "%" 'query-replace
    "&" 'async-shell-command
    "tT" 'e:toggle-indent-tabs-mode)
  (bind-keys*
   :map global-map
   ("C-;" . spacemacs/default-pop-shell)
   :map ctl-x-map
   ("C-c" . helm-M-x))
  :doc "マークを辿る"
  :config
  (set-variable 'set-mark-command-repeat-pop t)
  :doc "シェルの設定"
  :config
  (set-variable 'shell-file-name
                (or (executable-find "zsh")
                    (executable-find "bash")
                    (executable-find "sh")))
  :doc "パスワード関連"
  :config
  (set-variable 'epa-pinentry-mode 'loopback)
  (set-variable 'password-cache-expiry 3600)
  (set-variable 'plstore-encoded t)
  :doc "折り返し"
  :config
  (setq-default truncate-lines t)
  (set-variable 'truncate-partial-width-windows nil)
  :doc "最終行の改行"
  :config
  (set-variable 'mode-require-final-newline nil)
  (set-variable 'require-final-newline nil)
  :doc "ロックファイルを使用しない"
  :config
  (set-variable 'create-lockfiles nil)
  :doc "右から左に読む言語に対応しない"
  :config
  (setq-default bidi-display-reordering nil)
  :doc "コマンド履歴の重複を排除"
  :config
  (set-variable 'history-delete-duplicates t)
  :doc "特定のバッファを消去しない"
  :config
  (dolist (buffer '("*scratch*" "*Messages*"))
    (with-current-buffer buffer
      (emacs-lock-mode 'kill))))



(leaf configurations-for-Mac
  :if (spacemacs/system-is-mac)
  :doc "タイトルバーの見た目を変更"
  :config
  (let ((items '((ns-transparent-titlebar . t)
                 (ns-appearance . dark))))
    (dolist (item items)
      (assq-delete-all (car item) initial-frame-alist)
      (assq-delete-all (car item) default-frame-alist)
      (add-to-list 'initial-frame-alist item)
      (add-to-list 'default-frame-alist item)))
  :doc "特殊キーの設定"
  :config
  (set-variable 'ns-command-modifier 'meta)
  (set-variable 'ns-right-command-modifier 'super)
  (set-variable 'ns-alternate-modifier 'none))

(leaf configurations-for-WSL1/2
  :doc "WSLの情報を表示"
  :config
  (when (executable-find "uname")
    (let ((uname (kllib:shell-command-to-string "uname -a")))
      (cond
       ((s-index-of "microsoft-standard" uname)
        (set-variable 'dotspacemacs-frame-title-format "(WSL2) %I@%S"))
       ((s-index-of "Microsoft" uname)
        (set-variable 'dotspacemacs-frame-title-format "(WSL1) %I@%S")))))
  :doc "Windows側のブラウザを起動"
  :config
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (set-variable 'browse-url-generic-program  cmd-exe)
      (set-variable 'browse-url-generic-args     cmd-args)
      (set-variable 'browse-url-browser-function 'browse-url-generic))))

(leaf load-local-configurations
  :config
  (let ((private-config (expand-file-name "config" e:private-directory)))
    (condition-case err
        (load private-config)
      (error (message "Error: %s" err)))))



(leaf spacemacs
  :config
  (leaf core-dumper
    :doc "ダンプ処理に小細工を仕掛けていろいろ上手く調整"
    :defer-config
    (define-advice spacemacs/dump-emacs (:around (fn &rest args) trick)
      (let ((spacemacs-start-directory user-emacs-directory))
        (apply fn args)))))



(leaf ace-window
  :bind (("C-^" . ace-window))
  :init
  (spacemacs/set-leader-keys
    "^" 'ace-window)
  :config
  (set-variable 'aw-keys (number-sequence ?1 ?9))
  (set-variable 'aw-scope 'frame))

(leaf avy
  :defer-config
  (set-variable 'avy-keys (number-sequence ?a ?z))
  (set-variable 'avy-all-windows nil)
  (set-variable 'avy-all-windows-alt t))

(leaf bitwarden
  :defun (bitwarden--auto-cmd
          bitwarden--handle-message
          bitwarden-get
          bitwarden-get-field
          bitwarden-unlock
          bitwarden-unlocked-p)
  :config
  (defun bitwarden-get (account &optional key print-message)
    (unless (bitwarden-unlocked-p)
      (bitwarden-unlock))
    (let* ((args (list "get" "item" account))
           (json (bitwarden--handle-message (bitwarden--auto-cmd args) print-message))
           (result (json-parse-string json :object-type 'plist)))
      (if key
          (plist-get result key)
        result)))
  (defun bitwarden-get-field (account key)
    (let ((key (string-trim-left (format "%s" key) ":"))
          (result (bitwarden-get account :fields)))
      (plist-get (--first (string-equal (plist-get it :name) key)
                          (append result nil))
                 :value))))

(leaf *company
  :config
  (leaf company
    :bind (:company-active-map
           :package company
           ("C-g" . company-abort)
           ("<escape>" . company-abort))
    :init
    (defun e:setup-company-backends (backends)
      (let ((default '(company-dabbrev-code
                       company-files
                       company-dabbrev)))
        (setq-local company-backends (-concat (list backends) default))))
    :config
    (spacemacs|diminish company-mode))
  (leaf company-box
    :defer-config
    (spacemacs|diminish company-box-mode)))

(leaf *dired
  :config
  (leaf dired
    :bind ((:dired-mode-map
            ("C-c C-e" . wdired-change-to-wdired-mode)))
    :config
    (set-variable 'dired-dwim-target t)
    (set-variable 'dired-listing-switches "-Ahl")
    (set-variable 'dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.DS_Store")
    (set-variable 'dired-recursive-copies 'always)
    (set-variable 'dired-recursive-deletes 'always))
  (leaf image-dired
    :defer-config
    (e:place-in-cache image-dired-dir "dired/images"))
  (leaf ls-lisp
    :after dired
    :require t
    :config
    (set-variable 'ls-lisp-dirs-first t)
    (set-variable 'ls-lisp-format-time-list '("%Y-%m-%d %H:%M:%S" "%Y-%m-%d %H:%M:%S"))
    (set-variable 'ls-lisp-ignore-case nil)
    (set-variable 'ls-lisp-use-insert-directory-program nil)
    (set-variable 'ls-lisp-use-localized-time-format t)
    (set-variable 'ls-lisp-verbosity '(uid gid)))
  (leaf ls-lisp-extension
    :after ls-lisp
    :config
    (ls-lisp-extension-on)))

(leaf display-line-numbers
  :hook ((find-file-hook . e:display-line-numbers-mode-on)
         (prog-mode-hook . e:display-line-numbers-mode-on)
         (html-mode-hook . e:display-line-numbers-mode-on))
  :config
  (defun e:display-line-numbers-mode-on ()
    (interactive)
    (display-line-numbers-mode 1))
  (setq-default display-line-numbers-width 4))

(leaf eaw
  :require t
  :config
  (eaw-fullwidth))

(leaf ediff
  :commands (e:ediff)
  :config
  (defun e:ediff ()
    "画面分割されている場合にいい感じに EDIFF する."
    (interactive)
    (let ((files (->> (window-list (selected-frame))
                      (-map #'window-buffer)
                      (-map #'buffer-file-name)
                      (-non-nil))))
      (if (= (length files) 2)
          (ediff (nth 0 files)
                 (nth 1 files))
        (call-interactively #'ediff)))))

(leaf emmet-mode
  :bind (:emmet-mode-keymap
         ("<C-return>" . nil)
         ("C-c C-j" . emmet-expand-line)
         ("C-j" . nil)))

(leaf eshell
  :defer-config
  (set-variable 'eshell-history-size 100000))

(leaf *evil
  :config
  (leaf evil
    :require t
    :bind (;; motion → normal → visual
           (:evil-motion-state-map
            ("C-\\" . ignore)
            ("C-^" . nil))
           (:evil-normal-state-map
            ("<down>" . evil-next-visual-line)
            ("<up>" . evil-previous-visual-line)
            ("j" . evil-next-visual-line)
            ("k" . evil-previous-visual-line)
            ("gj" . evil-avy-goto-line-below)
            ("gk" . evil-avy-goto-line-above)
            ("S" . evil-avy-goto-char-timer))
           (:evil-visual-state-map)
           (:evil-insert-state-map)
           (:evil-operator-state-map)
           (:evil-replace-state-map)
           (:evil-emacs-state-map))
    :config
    (spacemacs|diminish hybrid-mode)
    (set-variable 'evil-cross-lines t)
    (set-variable 'evil-disable-insert-state-bindings t)
    (set-variable 'evil-move-beyond-eol t)
    (set-variable 'evil-move-cursor-back nil)
    :doc "保存時等にノーマルステートに戻す"
    :advice
    (:after  save-buffer   e:evil-force-normal-state)
    (:before keyboard-quit e:evil-force-normal-state)
    :config
    (defun e:evil-force-normal-state (&rest _)
      (cond
       ((eq evil-state 'visual)
        (evil-exit-visual-state))
       ((member evil-state '(insert hybrid))
        (evil-force-normal-state)))))
  (leaf evil-easymotion
    :config
    (define-prefix-command 'e:evil-em-command)
    (bind-keys
     :map e:evil-em-command
     ("w"  . ("em/forward-word-begin"        . evilem-motion-forward-word-begin))
     ("W"  . ("em/forward-WORD-begin"        . evilem-motion-forward-WORD-begin))
     ("e"  . ("em/forward-word-end"          . evilem-motion-forward-word-end))
     ("E"  . ("em/forward-WORD-end"          . evilem-motion-forward-WORD-end))
     ("b"  . ("em/backward-word-begin"       . evilem-motion-backward-word-begin))
     ("B"  . ("em/backward-WORD-begin"       . evilem-motion-backward-WORD-begin))
     ("j"  . ("em/next-visual-line"          . evilem-motion-next-visual-line))
     ("J"  . ("em/next-line"                 . evilem-motion-next-line))
     ("k"  . ("em/previous-visual-line"      . evilem-motion-previous-visual-line))
     ("K"  . ("em/previous-line"             . evilem-motion-previous-line))
     ("g"  . ("em/backward-word/WORD-end"))
     ("ge" . ("em/backward-word-end"         . evilem-motion-backward-word-end))
     ("gE" . ("em/backward-WORD-end"         . evilem-motion-backward-WORD-end))
     ("t"  . ("em/find-char-to"              . evilem-motion-find-char-to))
     ("T"  . ("em/find-char-to-backward"     . evilem-motion-find-char-to-backward))
     ("f"  . ("em/find-char"                 . evilem-motion-find-char))
     ("F"  . ("em/find-char-backward"        . evilem-motion-find-char-backward))
     ("["  . ("em/backward-section"))
     ("[[" . ("em/backward-section-begin"    . evilem-motion-backward-section-begin))
     ("[]" . ("em/backward-section-end"      . evilem-motion-backward-section-end))
     ("]"  . ("em/forward-section"))
     ("]]" . ("em/forward-section-begin"     . evilem-motion-forward-section-begin))
     ("][" . ("em/forward-section-end"       . evilem-motion-forward-section-end))
     ("("  . ("em/backward-section-begin"    . evilem-motion-backward-sentence-begin))
     (")"  . ("em/forward-section-begin"     . evilem-motion-forward-sentence-begin))
     ("n"  . ("em/search-next"               . evilem-motion-search-next))
     ("N"  . ("em/search-previous"           . evilem-motion-search-previous))
     ("*"  . ("em/search-word-forward"       . evilem-motion-search-word-forward))
     ("#"  . ("em/search-word-backward"      . evilem-motion-search-word-backward))
     ("-"  . ("em/pres-line-first-non-blank" . evilem-motion-previous-line-first-non-blank))
     ("+"  . ("em/next-line-first-non-blank" . evilem-motion-next-line-first-non-blank))
     ("s"  . evil-avy-goto-char-timer))
    (bind-key "s" 'e:evil-em-command evil-normal-state-map)
    (bind-key "x" 'e:evil-em-command evil-visual-state-map)
    (bind-key "x" 'e:evil-em-command evil-operator-state-map)))

(leaf flyspell
  :bind (:flyspell-mode-map
         ("C-;" . nil)))

(leaf flycheck
  :defer-config
  (set-variable 'flycheck-idle-buffer-switch-delay 3.0)
  (set-variable 'flycheck-idle-change-delay 3.0))

(leaf ggtags
  :defer-config
  (spacemacs|diminish ggtags-navigation-mode))

(leaf google-translate-default-ui
  :defer-config
  (set-variable 'google-translate-default-source-language "en")
  (set-variable 'google-translate-default-target-language "ja"))

(leaf *helm
  :config
  (leaf helm
    :bind (([remap eval-expression] . helm-eval-expression))
    :config
    (set-variable 'helm-buffer-max-length nil))
  (leaf helm-fzf
    :init
    (spacemacs/set-leader-keys
      "fz" 'helm-fzf
      "pz" 'helm-fzf-project-root)
    :defer-config
    (set-variable 'helm-fzf-args nil))
  (leaf helm-gtags
    :defer-config
    (spacemacs|diminish helm-gtags-mode))
  (leaf helm-multi-match
    :if (executable-find "cmigemo")
    :require t
    :config
    (helm-migemo-mode)
    (spacemacs|diminish helm-migemo-mode)))

(leaf highlight-indentation
  :hook ((haml-mode-hook . e:setup-highlight-indentation-mode)
         (yaml-mode-hook . e:setup-highlight-indentation-mode))
  :config
  (spacemacs|diminish highlight-indentation-mode)
  (spacemacs|diminish highlight-indentation-current-column-mode)
  (set-face-attribute 'highlight-indentation-face nil :background "#404040")
  (set-face-attribute 'highlight-indentation-current-column-face nil :background "#408040")
  (defun e:setup-highlight-indentation-mode ()
    (highlight-indentation-mode 1)
    (highlight-indentation-current-column-mode 1)
    (highlight-indentation-set-offset 2)))

(leaf *magit
  :defun (magit-add-section-hook)
  :init
  (add-to-list 'load-path (f-expand "libegit2" e:external-directory))
  :config
  (leaf magit
    :commands (magit-insert-skip-worktree-files)
    :config
    (set-variable 'magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 15))
    (set-variable 'magit-diff-refine-hunk 'all)
    (set-variable 'magit-diff-refine-ignore-whitespace t)
    (set-variable 'smerge-refine-ignore-whitespace nil)
    (magit-add-section-hook 'magit-status-sections-hook #'magit-insert-skip-worktree-files nil t)
    (evil-define-key 'normal magit-mode-map (kbd "<escape>") 'ignore))
  (leaf magit
    :if (executable-find "ghq")
    :defer-config
    (when (executable-find "ghq")
      (set-variable 'magit-repository-directories
                    (->> (kllib:shell-command-to-list "ghq root --all")
                         (--map (cons it 4))))))
  (leaf magit-delta
    :defer-config
    (spacemacs|diminish magit-delta-mode))
  (leaf transient
    :defer-config
    (set-variable 'transient-default-level 7)))

(leaf markdown-mode
  :defer-config
  (set-variable 'markdown-command "pandoc")
  (remove-hook 'markdown-mode-hook #'orgtbl-mode))

(leaf notmuch
  :defun (notmuch-bury-or-kill-this-buffer@kill-layout)
  :defer-config
  (prog1
      (defun notmuch-bury-or-kill-this-buffer@kill-layout
          (fn)
        (let
            ((kill
              (derived-mode-p 'notmuch-hello-mode)))
          (prog1
              (funcall fn)
            (if kill
                (persp-kill notmuch-spacemacs-layout-name)))))
    (advice-add 'notmuch-bury-or-kill-this-buffer :around
                (function notmuch-bury-or-kill-this-buffer@kill-layout)))
  (set-variable 'notmuch-archive-tags '("-inbox" "-unread"))
  (set-variable 'notmuch-message-deleted-tags '("+trash" "-inbox"))
  (set-variable 'notmuch-column-control 1.0)
  (set-variable 'notmuch-hello-thousands-separator ",")
  (set-variable 'notmuch-search-oldest-first nil)
  (set-variable 'notmuch-show-empty-saved-searches nil)
  (set-variable 'notmuch-show-logo nil)
  (set-variable 'notmuch-hello-hide-tags
                '(;; -------------------------
                  "drafts"    ;; +下書き
                  "flagged"   ;; +スター付き
                  "important" ;; +重要
                  "inbox"     ;; +受信トレイ
                  "sent"      ;; +送信済み
                  "spam"      ;; +迷惑メール
                  "trash"     ;; +ごみ箱
                  "unread"    ;; +未読
                  ;; -------------------------
                  "encrypted" ;; -暗号
                  "new"       ;; -新規(notmuch)
                  "signed"    ;; -署名
                  ;; -------------------------
                  ))
  (set-variable 'notmuch-saved-searches
                '((:name "すべて"     :query "*"             :key "a")
                  (:name "受信トレイ" :query "tag:inbox"     :key "i")
                  (:name "未読"       :query "tag:unread"    :key "u")
                  (:name "スター付き" :query "tag:flagged"   :key "s")
                  (:name "重要"       :query "tag:important" :key "m")
                  (:name "送信済み"   :query "tag:sent"      :key "t")
                  (:name "下書き"     :query "tag:draft"     :key "d")
                  (:name "ごみ箱"     :query "tag:trash")
                  (:name "迷惑メール" :query "tag:spam")))
  (setenv "XAPIAN_CJK_NGRAM" "1"))

(leaf open-junk-file
  :defer-config
  (set-variable 'open-junk-file-format (expand-file-name "junk/%Y/%Y%m%d-%H%M%S." e:private-directory)))

(leaf *org
  :config
  (leaf org
    :defvar (org-agenda-file-regexp org-babel-load-languages)
    :defer-config
    (set-variable 'org-directory (expand-file-name "org/" e:private-directory))
    (when (f-directory? org-directory)
      (set-variable 'org-default-notes-file (expand-file-name "index.org" org-directory))
      (set-variable 'org-agenda-files (-union (list org-default-notes-file)
                                              (directory-files-recursively org-directory org-agenda-file-regexp)))
      (set-variable 'org-refile-targets '((org-agenda-files :maxlevel . 3))))
    (set-variable 'org-todo-keywords
                  '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                    (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)")))
    (set-variable 'org-startup-indented t)
    (set-variable 'org-startup-folded nil)
    (set-variable 'org-indent-mode-turns-on-hiding-stars nil)
    (set-variable 'org-indent-indentation-per-level 2)
    (set-variable 'org-edit-src-content-indentation 0)
    (set-face-attribute 'org-level-1 nil :height 1.0)
    (set-face-attribute 'org-level-2 nil :height 1.0)
    (set-face-attribute 'org-level-3 nil :height 1.0))
  (leaf org-roam
    :config
    (set-variable 'org-roam-directory (expand-file-name "roam" org-directory))
    (e:place-in-cache org-roam-db-location "org-roam.db"))
  (leaf ob-restclient
    :after org
    :config
    (unless (--find (eq (car it) 'restclient) org-babel-load-languages)
      (org-babel-do-load-languages 'org-babel-load-languages
                                   (append org-babel-load-languages '((restclient . t)))))))

(leaf *skk
  :config
  (leaf skk
    :hook ((evil-hybrid-state-entry-hook . e:skk-mode)
           (evil-hybrid-state-exit-hook  . skk-mode-exit))
    :bind (([remap toggle-input-method] . skk-mode)
           ("C-¥" . skk-mode))
    :init
    (set-variable 'default-input-method "japanese-skk")
    (set-variable 'skk-user-directory (expand-file-name "ddskk" e:private-directory))
    (set-variable 'skk-large-jisyo (expand-file-name "dic-mirror/SKK-JISYO.L" e:external-directory))
    (set-variable 'skk-preload t)
    (set-variable 'skk-egg-like-newline t)
    (set-variable 'skk-share-private-jisyo t)
    (set-variable 'skk-show-annotation t)
    (set-variable 'skk-show-inline 'vertical)
    (set-variable 'skk-sticky-key ";")
    (set-variable 'skk-use-jisx0201-input-method t)
    :config
    (defun e:skk-mode ()
      "skk の有効化で半角英数入力にする"
      (interactive)
      (unless (member major-mode '(vterm-mode))
        (if (bound-and-true-p skk-mode)
            (skk-latin-mode-on)
          (let ((skk-mode-hook (-union skk-mode-hook '(skk-latin-mode-on))))
            (skk-mode))))))
  (leaf google-ime-skk
    :if (executable-find "google-ime-skk")
    :require prodigy
    :commands (e:prodigy:google-ime-skk)
    :doc "設定"
    :config
    (set-variable 'skk-server-prog (executable-find "google-ime-skk"))
    (set-variable 'skk-server-inhibit-startup-server t)
    (set-variable 'skk-server-host "127.0.0.1")
    (set-variable 'skk-server-portnum 55100)
    :doc "起動"
    :config
    (defun e:prodigy:google-ime-skk ()
      (interactive)
      (let ((service "google-ime-skk"))
        (unless (prodigy-find-service service)
          (prodigy-define-service
            :name service
            :command "google-ime-skk"
            :tags '(general)
            :kill-signal 'sigkill))
        (e:prodigy-start-service service)))
    (spacemacs/defer-until-after-user-config  #'e:prodigy:google-ime-skk)))

(leaf paradox-github
  :defer-config
  (set-variable 'paradox-github-token (e:auth-source-get 'token :host "paradox")))

(leaf prodigy
  :commands (e:prodigy-start-service)
  :config
  (leaf e:prodigy-start-service
    :config
    (defun e:prodigy-start-service (name)
      (let ((service (prodigy-find-service name)))
        (when service
          (prodigy-start-service service))))))

(leaf projectile
  :if (executable-find "ghq")
  :defer-config
  (setq projectile-known-projects
        (->> projectile-known-projects
             (--remove (eq (projectile-project-vcs it) 'none))
             (-union (-map #'f-short (kllib:shell-command-to-list "ghq list --full-path")))
             (-map #'file-name-as-directory)
             (-sort #'s-less?)
             (-uniq))))

(leaf persp-mode
  :defer-config
  (set-variable 'persp-kill-foreign-buffer-behaviour nil))

(leaf shell-pop
  :defer-config
  (set-variable 'shell-pop-autocd-to-working-dir nil))

(leaf shr
  :defer-config
  (set-variable 'shr-use-colors nil)
  (set-variable 'shr-max-image-proportion 0.6))

(leaf smartparens
  :defer-config
  (spacemacs|diminish smartparens-mode))

(leaf so-long
  :config
  (global-so-long-mode 1))

(leaf recentf
  :advice
  (:before recentf-save-list e:recentf-save-list--cleanup)
  :defer-config
  (set-variable 'recentf-max-menu-items 20)
  (set-variable 'recentf-max-saved-items 3000)
  (set-variable 'recentf-filename-handlers '(abbreviate-file-name))
  (defun e:recentf-save-list--cleanup (&rest _)
    "存在しないファイルを履歴から削除する"
    (let ((items recentf-list)
          (ignore `(,(f-short recentf-save-file) ,null-device)))
      (setq items (-map 'f-short items))
      (setq items (-distinct items))
      (setq items (--filter (or (file-remote-p it)
                                (f-exists? it))
                            items))
      (setq items (-difference items ignore))
      (setq recentf-list items))))

(leaf tramp
  :require t
  :config
  (set-variable 'tramp-default-host "localhost")
  :doc "ssh/conf.d の中身から接続先を追加"
  :config
  (when (f-exists? "~/.ssh/conf.d/hosts")
    (let ((functions (->> (ignore-errors (f-files "~/.ssh/conf.d/hosts" nil t))
                          (--map (list #'tramp-parse-sconfig it)))))
      (--each '("ssh" "scp")
        (let ((new-functions (-union (tramp-get-completion-function it) functions)))
          (tramp-set-completion-function it new-functions))))))

(leaf treemacs
  :defer-config
  (e:place-in-cache treemacs-persist-file "treemacs/persist")
  (e:place-in-cache treemacs-last-error-persist-file "treemacs/persist-at-last-error"))

(leaf url-cache
  :defer-config
  (e:place-in-cache url-cache-directory "url/cache"))

(leaf url-cookie
  :defer-config
  (e:place-in-cache url-cookie-file "url/cookies"))

(leaf vterm
  :commands (vterm-yank)
  :hook (vterm-mode-hook . e:vterm-auto-tmux)
  :bind (:vterm-mode-map
         ("C-c C-g" . keyboard-quit)
         ("C-g" . vterm-send-C-g)
         ("C-j" . e:vterm-input-something))
  :config
  (evil-define-key 'hybrid vterm-mode-map (kbd "<escape>") #'vterm-send-escape)
  (defun e:vterm-input-something ()
    (interactive)
    (let ((input (read-string "input: ")))
      (with-no-warnings (vterm-send-string input))))
  (defun e:vterm-auto-tmux ()
    (with-no-warnings
      (vterm-send-string "exec tmux new -A -s emacs")
      (vterm-send-return)))
  (set-variable 'vterm-max-scrollback 20000)
  (set-face-attribute 'vterm-color-black   nil :foreground "#073642" :background "#002b36")
  (set-face-attribute 'vterm-color-red     nil :foreground "#dc322f" :background "#cb4b16")
  (set-face-attribute 'vterm-color-green   nil :foreground "#859900" :background "#586e75")
  (set-face-attribute 'vterm-color-yellow  nil :foreground "#b58900" :background "#657b83")
  (set-face-attribute 'vterm-color-blue    nil :foreground "#268bd2" :background "#839496")
  (set-face-attribute 'vterm-color-magenta nil :foreground "#d33682" :background "#6c71c4")
  (set-face-attribute 'vterm-color-cyan    nil :foreground "#2aa198" :background "#93a1a1")
  (set-face-attribute 'vterm-color-white   nil :foreground "#eee8d5" :background "#fdf6e3"))

(leaf which-key
  :defer-config
  (spacemacs|diminish which-key-mode))

(leaf whitespace
  :hook ((find-file-hook prog-mode-hook) . whitespace-mode-on)
  :config
  (spacemacs|diminish whitespace-mode)
  (defun whitespace-mode-on ()
    (interactive)
    (whitespace-mode 1))
  (set-variable 'whitespace-style
                '(face
                  trailing
                  tabs
                  tab-mark
                  spaces
                  space-mark
                  newline
                  newline-mark))
  (set-variable 'whitespace-space-regexp "\\(\u3000+\\)")
  (set-variable 'whitespace-display-mappings
                '((space-mark   ?\u3000 [?\u30ed])
                  (tab-mark     ?\t     [?\t])
                  (newline-mark ?\n     [?\u0024 ?\n])))
  (set-face-attribute 'whitespace-trailing nil :background "#800000")
  (let ((color "#595D63"))
    (set-face-attribute 'whitespace-tab      nil :foreground color :strike-through t)
    (set-face-attribute 'whitespace-space    nil :foreground color)
    (set-face-attribute 'whitespace-newline  nil :foreground color)))

(leaf yasnippet
  :defer-config
  (spacemacs|diminish yas-minor-mode)
  (set-variable 'yas-snippet-dirs e:yas-snippet-dirs)
  (yas-reload-all))



(require 'user-config-lsp)
(require 'user-config-js)
(require 'user-config-php)
(require 'user-config-ruby)



(provide 'user-config)

;;; user-config.el ends here
