;;; user-config.el

(eval-and-compile
  (require 'leaf)
  (require 'komunan-lisp-library)
  (require 'user-macros))

(leaf custom-headerline
  :commands (custom-headerline-start)
  :init
  (spacemacs/defer-until-after-user-config 'custom-headerline-start))

(leaf setup:envinronment
  :doc "日本語環境"
  :config
  (set-language-environment "Japanese")
  :doc "Encoding"
  :config
  (let ((coding-system 'utf-8))
    (prefer-coding-system          coding-system)
    (set-buffer-file-coding-system coding-system))
  :doc "Locale"
  (let ((value "ja_JP.UTF-8"))
    (setenv "LANG" value)
    (setenv "LC_ALL" value)))

(leaf setup:spacemacs
  :doc "ダンプ処理に小細工を仕掛けていろいろ上手く調整"
  :config
  (leaf core-dumper
    :defer-config
    (define-advice spacemacs/dump-emacs (:around (fn &rest args) trick)
      (let ((spacemacs-start-directory user-emacs-directory))
        (apply fn args))))
  :doc "フォント調整"
  :config
  (let ((font (car dotspacemacs-default-font)))
    (set-face-attribute 'fixed-pitch    nil :family font)
    (set-face-attribute 'variable-pitch nil :family font))
  :doc "タイトル表示"
  :config
  (defun custom-frame-title-format ()
    (if (and (fboundp 'org-clocking-p)
             (org-clocking-p))
        org-mode-line-string
      (spacemacs/title-prepare dotspacemacs-frame-title-format)))
  (e:variable! frame-title-format '(:eval (custom-frame-title-format)))
  :doc "モードラインの表示を切替え"
  :config
  (e:variable! spaceline-purpose-p nil)
  (e:variable! spaceline-selection-info-p nil)
  (e:variable! spaceline-version-control-p nil)
  :doc "モードラインの表示を改善"
  :config
  (spaceline-define-segment buffer-modified
    "Buffer modified marker."
    (cond
     (buffer-read-only    "")
     ((buffer-modified-p) "")))
  (spaceline-define-segment buffer-encoding-abbrev
    "The line ending convention used in the buffer."
    (let ((buf-coding (format "%s" buffer-file-coding-system)))
      (list (string-trim-right buf-coding (rx (+ "-" (or "with-signature" "unix" "dos" "mac")) eol))
            (concat (and (string-match "with-signature" buf-coding) "ⓑ")
                    (and (string-match "unix"           buf-coding) "ⓤ")
                    (and (string-match "dos"            buf-coding) "ⓓ")
                    (and (string-match "mac"            buf-coding) "ⓜ")
                    )))
    :separator " "))

(leaf setup:custom-aliases
  :config
  (defalias 'exit 'save-buffers-kill-terminal)
  (defalias 'yes-or-no-p 'y-or-n-p))

(leaf setup:custom-keybindings
  :config
  (spacemacs/set-leader-keys
    "%" 'query-replace
    "&" 'async-shell-command
    ":" 'popwin:daily-report
    "^" 'ace-window
    "|" 'shell-command-on-region)
  (bind-keys*
   :map global-map
   ("C-:" . popwin:daily-report)
   ("C-*" . popwin:note)
   ("C-;" . spacemacs/default-pop-shell)
   ("C-<" . evil-jump-backward)
   ("C->" . evil-jump-forward)
   ("C-^" . ace-window)
   :map ctl-x-map
   ("C-c" . execute-extended-command)))

(leaf setup:emacs-config
  :doc "C Sources"
  :config
  (e:default! bidi-display-reordering nil)
  (e:default! fill-column 100)
  (e:default! truncate-lines t)
  (e:variable! create-lockfiles nil)
  (e:variable! delete-by-moving-to-trash nil)
  (e:variable! frame-resize-pixelwise t)
  (e:variable! history-delete-duplicates t)
  (e:variable! shell-file-name "zsh")
  (e:variable! system-time-locale "C")
  (e:variable! truncate-partial-width-windows nil)
  (e:variable! window-resize-pixelwise t)
  (spacemacs|add-toggle indent-tabs-mode
    :status indent-tabs-mode
    :on  (setq indent-tabs-mode t)
    :off (setq indent-tabs-mode nil)
    :evil-leader "tT"))

(leaf setup:mac
  :if (spacemacs/system-is-mac)
  :doc "タイトルバーの見た目を変更"
  :config
  (--each '((ns-transparent-titlebar . t)
            (ns-appearance . dark))
    (assq-delete-all (car it) initial-frame-alist)
    (assq-delete-all (car it) default-frame-alist)
    (add-to-list 'initial-frame-alist it)
    (add-to-list 'default-frame-alist it))
  :doc "特殊キーの設定"
  :config
  (e:variable! ns-command-modifier 'meta)
  (e:variable! ns-right-command-modifier 'super)
  (e:variable! ns-alternate-modifier 'none)
  :doc "ちょっと行間を広げる"
  :config
  (e:default! line-spacing 2))

(leaf setup:WSL1/2
  :doc "Windows側のブラウザを起動"
  :config
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (e:variable! browse-url-generic-program cmd-exe)
      (e:variable! browse-url-generic-args    cmd-args))))

(leaf setup:private-config
  :config
  (let ((private-config (f-expand "config" e:private-directory)))
    (condition-case err
        (load private-config)
      (display-warning :warning err))))

(leaf ace-window
  :defer-config
  (e:variable! aw-keys (number-sequence ?1 ?9))
  (e:variable! aw-scope 'frame))

(leaf auth-source
  :commands (e:auth-source-get)
  :config
  (e:variable! auth-sources '("~/.authinfo.json.gpg"))
  (defun e:auth-source-get (prop &rest spec)
    (plist-get (car (apply 'auth-source-search spec)) prop)))

(leaf avy
  :defer-config
  (e:variable! avy-all-windows nil)
  (e:variable! avy-all-windows-alt t)
  (e:variable! avy-style 'de-bruijn))

(leaf browse-url
  :commands (browse-url-by-choosen)
  :config
  (e:variable! browse-url-browser-function 'browse-url-by-choosen)
  (defun browse-url-by-choosen (url &optional new-window)
    "選択したブラウザで URL を開く."
    (let ((browsers '(eww-browse-url browse-url-default-browser xwidget-webkit-browse-url)))
      (when browse-url-generic-program
        (add-to-list 'browsers 'browse-url-generic t))
      (funcall (intern (completing-read "Choose Browser: " browsers)) url new-window))))

(leaf codic
  :defer-config
  (e:variable! codic-api-token (e:auth-source-get :token :host "emacs" :user "codic")))

(leaf company
  :bind (:company-active-map
         :package company
         ("C-g" . company-abort)
         ("<escape>" . company-abort)
         ("<backspace>" . delete-backward-char-and-company-abort))
  :init
  (spacemacs|use-package-add-hook company-box
    :post-config
    (e:variable! company-box-backends-colors
                 '((company-capf    . (:candidate "#90ee90"))
                   (company-tabnine . (:candidate "#696969"))))
    (e:variable! company-box-icon-right-margin 0.5)
    (e:variable! company-box-icons-alist 'company-box-icons-images))
  :config
  (spacemacs|diminish company-mode)
  (spacemacs|diminish company-box-mode)
  (e:variable! company-transformers '(company-prescient-transformer))
  (defun delete-backward-char-and-company-abort ()
    (interactive)
    (delete-char -1)
    (company-abort)))

(leaf devdocs
  :config
  (set-variable 'devdocs-data-dir (expand-file-name "devdocs" spacemacs-cache-directory))
  (spacemacs/declare-prefix "hdd" "devdocs")
  (spacemacs/set-leader-keys
    "hddd" 'devdocs-lookup
    "hddi" 'devdocs-install
    "hddl" 'devdocs-lookup
    "hddp" 'devdocs-peruse
    "hddq" 'devdocs-lookup
    "hdds" 'devdocs-search
    "hddu" 'devdocs-update-all
    ))

(leaf dired
  :bind ((:dired-mode-map
          ("C-c C-e" . wdired-change-to-wdired-mode)))
  :config
  (e:cache! image-dired-dir "dired/images")
  (e:variable! dired-dwim-target t)
  (e:variable! dired-listing-switches "-Ahl")
  (e:variable! dired-omit-files (rx (or (seq bol (? ".") "#")
                                        (seq bol (or "." "..") eol)
                                        (seq bol ".DS_Store" eol))))
  (e:variable! dired-recursive-copies 'always)
  (e:variable! dired-recursive-deletes 'always)
  (e:variable! ls-lisp-dirs-first t)
  (e:variable! ls-lisp-format-time-list '("%Y-%m-%d %H:%M:%S" "%Y-%m-%d %H:%M:%S"))
  (e:variable! ls-lisp-ignore-case nil)
  (e:variable! ls-lisp-use-insert-directory-program nil)
  (e:variable! ls-lisp-use-localized-time-format t)
  (e:variable! ls-lisp-verbosity '(uid gid))
  (ls-lisp-extension-on))

(leaf display-line-numbers
  :hook ((find-file-hook . spacemacs/toggle-display-line-numbers-mode-on)
         (html-mode-hook . spacemacs/toggle-display-line-numbers-mode-on)
         (org-mode-hook  . spacemacs/toggle-display-line-numbers-mode-on)
         (prog-mode-hook . spacemacs/toggle-display-line-numbers-mode-on))
  :config
  (e:default! display-line-numbers-width 4)
  (spacemacs|add-toggle display-line-numbers-mode
    :status display-line-numbers-mode
    :on  (display-line-numbers-mode 1)
    :off (display-line-numbers-mode 0)))

(leaf eaw
  :commands (eaw-fullwidth)
  :init
  (spacemacs/defer-until-after-user-config 'eaw-fullwidth))

(leaf emacs-lock
  :config
  (dolist (buffer '("*scratch*" "*Messages*"))
    (with-current-buffer buffer
      (emacs-lock-mode 'kill))))

(leaf emmet-mode
  :bind (:emmet-mode-keymap
         ("<C-return>" . nil)
         ("C-c C-j" . emmet-expand-line)
         ("C-j" . nil)))

(leaf epg-config
  :defer-config
  (e:variable! epg-pinentry-mode 'loopback))

(leaf eshell
  :defer-config
  (e:variable! eshell-history-size 100000))

(leaf evil
  :require t
  :bind (;; motion → normal → visual
         (:evil-motion-state-map
          ("C-\\" . ignore)
          ("C-^"  . nil))
         (:evil-normal-state-map
          ("<down>" . evil-next-visual-line)
          ("<up>"   . evil-previous-visual-line)
          ("j"  . evil-next-visual-line)
          ("k"  . evil-previous-visual-line)
          ("gj" . evil-avy-goto-line-below)
          ("gk" . evil-avy-goto-line-above)
          ("S"  . evil-avy-goto-word-0))
         (:evil-visual-state-map)
         (:evil-insert-state-map)
         (:evil-operator-state-map)
         (:evil-replace-state-map)
         (:evil-emacs-state-map))
  :config
  (spacemacs|diminish hybrid-mode)
  (e:variable! evil-cross-lines t)
  (e:variable! evil-disable-insert-state-bindings t)
  (e:variable! evil-move-beyond-eol t)
  (e:variable! evil-move-cursor-back nil)
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
  (bind-key "x" 'e:evil-em-command evil-operator-state-map))

(leaf eww
  :bind (:eww-mode-map
         ("e" . eww-open-current-url-with-default-browser))
  :defun (eww-current-url)
  :config
  (e:variable! eww-search-prefix "https://www.google.com/search?q=")
  (defun eww-open-current-url-with-default-browser ()
    (interactive)
    (browse-url-default-browser (eww-current-url))))

(leaf files
  :defer-config
  (e:variable! mode-require-final-newline nil)
  (e:variable! require-final-newline nil))

(leaf flycheck
  :defer-config
  (e:variable! flycheck-idle-buffer-switch-delay 3.0)
  (e:variable! flycheck-idle-change-delay 3.0)
  (e:variable! flycheck-temp-prefix ".flycheck")
  (spacemacs/set-leader-keys
    "eY" 'e:flycheck-copy-error-ids)
  (defun e:flycheck-copy-error-ids ()
    (interactive)
    (let ((messages (->> (flycheck-overlay-errors-at (point))
                         (-map #'flycheck-error-id)
                         (-non-nil))))
      (when messages
        (kill-new (string-join messages ", "))
        (message (string-join messages ", ")))))
  (define-advice flycheck-buffer (:before (&rest _) after-clear)
    (flycheck-clear))
  (define-advice flycheck-error-list-refresh (:after (&rest _) fix-error-id)
    (--map (let ((id (flycheck-error-id it)))
             (when (ht? id)
               (when-let* ((value (ht-get id "value"))
                           (_target (ht-get id "target")))
                 (setf (flycheck-error-id it)
                       (format "%s" value)))))
           (flycheck-overlay-errors-in (point-min) (point-max)))))

(leaf ggtags
  :defer-config
  (spacemacs|diminish ggtags-navigation-mode))

(leaf google-translate-default-ui
  :defer-config
  (e:variable! google-translate-default-source-language "en")
  (e:variable! google-translate-default-target-language "ja"))

(leaf helm
  :bind (([remap eval-expression] . helm-eval-expression-with-eldoc))
  :config
  (spacemacs|diminish helm-gtags-mode)
  (e:variable! helm-buffer-max-length nil)
  (when (executable-find "cmigemo")
    (spacemacs|diminish helm-migemo-mode)
    (helm-migemo-mode)))

(leaf helm-fzf
  :init
  (spacemacs/set-leader-keys
    "fz" 'helm-fzf
    "pz" 'helm-fzf-project-root)
  :defer-config
  (e:variable! helm-fzf-args '("--tac")))

(leaf highlight-indent-guide
  :config
  (spacemacs|diminish highlight-indent-guides-mode))

(leaf indent
  :defer-config
  (e:variable! standard-indent 2))

(leaf magit
  :defer-config
  (evil-define-key 'normal magit-mode-map (kbd "<escape>") 'ignore)
  (e:variable! magit-delete-by-moving-to-trash nil)
  (e:variable! magit-diff-refine-hunk 'all)
  (e:variable! magit-diff-refine-ignore-whitespace t)
  (e:variable! magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 15))
  (e:variable! smerge-refine-ignore-whitespace nil)
  (e:variable! transient-default-level 7)
  (--each '(magit-insert-skip-worktree-files magit-insert-modules-overview)
    (magit-add-section-hook 'magit-status-sections-hook it 'magit-insert-unpulled-from-upstream t))
  :doc "リポジトリの一覧表示にパスをつける"
  :defer-config
  (define-advice magit-repos-alist (:override (&rest _) override)
    (magit-list-repos-uniquify
     (--map (cons (f-short it) it)
            (magit-list-repos))))
  :doc "ghq で管理しているディレクトリを探索の対象にする"
  :defer-config
  (when (executable-find "ghq")
    (e:variable! magit-repository-directories
                 (->> (kllib:shell-command-to-list "ghq root --all")
                      (--map (cons it 5))))))

(leaf magit-todos
  :defer-config
  (e:variable! magit-todos-auto-group-items 100))

(leaf markdown-mode
  :defer-config
  (e:variable! markdown-command "pandoc"))

(leaf notmuch
  :defun (notmuch-bury-or-kill-this-buffer@kill-layout)
  :init
  (setenv "XAPIAN_CJK_NGRAM" "1")
  :defer-config
  (define-advice notmuch-bury-or-kill-this-buffer (:around (fn) kill-layout)
    (let ((kill (derived-mode-p 'notmuch-hello-mode)))
      (prog1
          (funcall fn)
        (if kill
            (persp-kill notmuch-spacemacs-layout-name)))))
  (setq-default notmuch-search-oldest-first nil)
  (e:variable! notmuch-archive-tags '("-inbox" "-unread"))
  (e:variable! notmuch-message-deleted-tags '("+trash" "-inbox"))
  (e:variable! notmuch-column-control 1.0)
  (e:variable! notmuch-hello-thousands-separator ",")
  (e:variable! notmuch-show-empty-saved-searches nil)
  (e:variable! notmuch-show-logo nil)
  (e:variable! notmuch-hello-hide-tags
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
  (e:variable! notmuch-saved-searches
               '((:name "すべて"     :query "*"             :key "a")
                 (:name "受信トレイ" :query "tag:inbox"     :key "i")
                 (:name "未読"       :query "tag:unread"    :key "u")
                 (:name "スター付き" :query "tag:flagged"   :key "s")
                 (:name "重要"       :query "tag:important" :key "m")
                 (:name "送信済み"   :query "tag:sent"      :key "t")
                 (:name "下書き"     :query "tag:draft"     :key "d")
                 (:name "ごみ箱"     :query "tag:trash")
                 (:name "迷惑メール" :query "tag:spam"))))
(leaf org
  :config
  (e:variable! org-agenda-current-time-string "← now")
  (e:variable! org-agenda-entry-text-leaders (s-concat (s-repeat 25 " ") "│ "))
  (e:variable! org-agenda-entry-text-maxlines 20)
  (e:variable! org-agenda-files (list (f-expand "~/org/daily/")))
  (e:variable! org-agenda-span 'day)
  (e:variable! org-default-notes-file (f-expand "~/org/index.org"))
  (e:variable! org-directory (f-expand "~/org/"))
  (e:variable! org-edit-src-content-indentation 0)
  (e:variable! org-indent-indentation-per-level 2)
  (e:variable! org-indent-mode-turns-on-hiding-stars nil)
  (e:variable! org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (e:variable! org-src-window-setup 'split-window-below)
  (e:variable! org-startup-folded nil)
  (e:variable! org-startup-indented t)
  (e:variable! org-tags-column 0)
  (e:variable! org-agenda-time-grid '((daily today require-timed)
                                      (800 1000 1200 1400 1600 1800 2000)
                                      "      "
                                      "────────────────"))
  (e:variable! org-todo-keyword-faces '(("TODO" . org-warning)
                                        ("WAITING" . org-done)
                                        ("HOLD" . org-done)))
  (e:variable! org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                                   (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)")))
  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-todo nil :foreground "#00ff00")
    (set-face-attribute 'org-done nil :foreground "#696969")
    (set-face-attribute 'org-headline-done nil :foreground "#696969")
    (set-face-attribute 'org-headline-todo nil :foreground "#00ff00")
    (set-face-attribute 'org-level-1 nil :height 1.0)
    (set-face-attribute 'org-level-2 nil :height 1.0)
    (set-face-attribute 'org-level-3 nil :height 1.0))
  ;; 経過時間の保存
  (e:variable! org-clock-persist t)
  (org-clock-persistence-insinuate)
  ;; 日報用(暫定)
  (defun org-support/daily-file ()
    (let* ((daily-dir (f-expand "daily" org-directory)))
      (f-short (f-expand (format-time-string "%Y.org") daily-dir))))
  (defun popwin:note ()
    (interactive)
    (popwin:popup-buffer (find-file-noselect (f-expand "note.org" org-directory)) :height 30 :dedicated t :stick t))
  (defun popwin:daily-report ()
    (interactive)
    (popwin:popup-buffer (find-file-noselect (org-support/daily-file)) :height 30 :dedicated t :stick t)))

;; FIXME: 消すと正常に起動しなくなる(どこかに変な依存があるかも)
(leaf org-roam
  :config
  (set-variable 'org-roam-directory (expand-file-name "~/org/roam"))
  (set-variable 'org-roam-db-location (expand-file-name "org-roam.db" spacemacs-cache-directory))
  (set-variable 'org-roam-v2-ack t)
  (org-roam-db-autosync-enable))

(leaf open-junk-file
  :defer-config
  (e:variable! open-junk-file-format (f-expand "junk/%Y/%Y%m%d-%H%M%S." e:private-directory)))

(leaf paradox-github
  :defer-config
  (e:variable! paradox-column-width-package 30)
  (e:variable! paradox-column-width-version 13)
  (e:variable! paradox-column-width-star 5)
  (e:variable! paradox-github-token (e:auth-source-get :token :host "github.com" :user "paradox")))

(leaf password-cache
  :defer-config
  (e:variable! password-cache-expiry 3600))

(leaf persistent-scratch
  :defer-config
  (define-advice persistent-scratch-save (:after (&rest _) saved)
    (--each (buffer-list)
      (with-current-buffer it
        (when (funcall persistent-scratch-scratch-buffer-p-function)
          (when (called-interactively-p 'any)
            (e:evil-force-normal-state))
          (set-buffer-modified-p nil))))))

(leaf persp-mode
  :defer-config
  (e:variable! persp-kill-foreign-buffer-behaviour nil))

(leaf popwin
  :defer-config
  (--each '("*Async Shell Command*" "*Warnings*")
    (push (list it :dedicated t :position 'bottom :stick t :noselect t) popwin:special-display-config)))

(leaf prodigy
  :commands (e:prodigy-start-service)
  :config
  (e:variable! prodigy-view-buffer-maximum-size 2048)
  (e:variable! prodigy-view-truncate-by-default t)
  (prodigy-define-tag
    :name 'rails
    :ready-message "Use Ctrl-C to stop")
  (defun e:prodigy-start-service (name)
    (let ((service (prodigy-find-service name)))
      (when service
        (prodigy-start-service service)))))

(leaf projectile
  :defer-config
  (e:variable! projectile-completion-system 'default)
  (defun e:setup-projectile-known-projects ()
    (when (executable-find "ghq")
      (setq projectile-known-projects
            (->> projectile-known-projects
                 (--remove (eq (projectile-project-vcs it) 'none))
                 (-union (-map 'f-short (kllib:shell-command-to-list "ghq list --full-path")))
                 (-map 'file-name-as-directory)
                 (-sort 's-less?)
                 (-distinct)))))
  (spacemacs/defer-until-after-user-config 'e:setup-projectile-known-projects))

(leaf shell-pop
  :defer-config
  (e:variable! shell-pop-autocd-to-working-dir nil))

(leaf shr
  :defer-config
  (e:variable! shr-use-fonts nil)
  (e:variable! shr-use-colors nil)
  (e:variable! shr-max-image-proportion 0.6))

(leaf simple
  :defer-config
  (e:variable! set-mark-command-repeat-pop t))

(leaf skk
  :hook ((evil-hybrid-state-entry-hook . spacemacs/toggle-skk-mode-on)
         (evil-hybrid-state-exit-hook  . spacemacs/toggle-skk-mode-off))
  :bind (([remap toggle-input-method] . skk-mode)
         ("C-¥" . skk-mode))
  :init
  (e:cache! skk-user-directory "ddskk")
  (e:variable! default-input-method "japanese-skk")
  (e:variable! skk-large-jisyo (f-expand "dic-mirror/SKK-JISYO.L" e:external-directory))
  (e:variable! skk-preload t)
  (e:variable! skk-egg-like-newline t)
  (e:variable! skk-share-private-jisyo t)
  (e:variable! skk-show-annotation t)
  (e:variable! skk-sticky-key ";")
  (e:variable! skk-use-jisx0201-input-method t)
  (e:variable! skk-server-prog (executable-find "yaskkserv2"))
  (e:variable! yaskkserv2-dictionary (f-expand "~/sync/share/dictionary.yaskkserv2"))
  :config
  (spacemacs|add-toggle skk-mode
    :status skk-mode
    :on  (e:skk-mode)
    :off (skk-mode-exit))
  (defun e:skk-mode ()
    "skk の有効化で半角英数入力にする"
    (interactive)
    (unless (member major-mode '(vterm-mode))
      (if (bound-and-true-p skk-mode)
          (skk-latin-mode-on)
        (let ((skk-mode-hook (-union skk-mode-hook '(skk-latin-mode-on))))
          (skk-mode)))))
  (when (and skk-server-prog
             (f-exists? yaskkserv2-dictionary))
    (e:variable! skk-large-jisyo nil)
    (e:variable! skk-server-inhibit-startup-server t)
    (e:variable! skk-server-host "127.0.0.1")
    (e:variable! skk-server-portnum 1178)
    (defun e:prodigy:yaskkserv2 ()
      (interactive)
      (when (require 'prodigy nil t)
        (let ((service "yaskkserv2"))
          (unless (prodigy-find-service service)
            (prodigy-define-service
              :name service
              :command skk-server-prog
              :args `("--no-daemonize" "--google-suggest" ,yaskkserv2-dictionary)
              :tags '(general)
              :stop-signal 'int))
          (e:prodigy-start-service service))))
    (spacemacs/defer-until-after-user-config  'e:prodigy:yaskkserv2)))

(leaf so-long
  :init
  (spacemacs/defer-until-after-user-config 'global-so-long-mode))

(leaf smartparens
  :defer-config
  (spacemacs|diminish smartparens-mode))

(leaf svg-lib
  :defer-config
  (e:cache! svg-lib-icons-dir "svg-lib"))

(leaf recentf
  :defer-config
  (e:variable! recentf-filename-handlers '(abbreviate-file-name))
  (define-advice recentf-save-list (:before (&rest _) cleanup)
    "存在しないファイルを履歴から削除する"
    (setq recentf-list (->> recentf-list
                            (-map 'f-short)
                            (-distinct)
                            (--filter (and (or (file-remote-p it)
                                               (f-exists? it))
                                           (recentf-include-p it)))))))

(leaf tramp
  :config
  (e:variable! tramp-default-host "localhost")
  (defun e:setup-tramp-completion ()
    "ssh/conf.d の中身から接続先を追加"
    (when (and (require 'tramp)
               (f-exists? "~/.ssh/conf.d/hosts"))
      (let ((functions (->> (ignore-errors (f-files "~/.ssh/conf.d/hosts" nil t))
                            (--map (list 'tramp-parse-sconfig it)))))
        (--each '("ssh" "scp")
          (let ((new-functions (-union (tramp-get-completion-function it) functions)))
            (tramp-set-completion-function it new-functions))))))
  (spacemacs/defer-until-after-user-config 'e:setup-tramp-completion))

(leaf tree-sitter
  :defer-config
  (spacemacs|diminish tree-sitter-mode ""))

(leaf treemacs
  :defer-config
  (e:cache! treemacs-persist-file "treemacs/persist")
  (e:cache! treemacs-last-error-persist-file "treemacs/persist-at-last-error"))

(leaf undo-tree
  :defer-config
  (e:variable! undo-tree-auto-save-history nil)
  (e:variable! undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))))

(leaf url-cache
  :defer-config
  (e:cache! url-cache-directory "url/cache"))

(leaf url-cookie
  :defer-config
  (e:cache! url-cookie-file "url/cookies"))

(leaf vc
  :config
  (spacemacs/set-leader-keys
    "gvh" 'vc-region-history))

(leaf vterm
  :bind (:vterm-mode-map
         ("C-c C-g" . keyboard-quit)
         ("C-g" . vterm-send-C-g)
         ("C-j" . e:vterm-input-something)
         ("<wheel-up>" . ignore)
         ("<wheel-down>" . ignore))
  :config
  (evil-define-key 'hybrid vterm-mode-map (kbd "<escape>") 'vterm-send-escape)
  (e:variable! vterm-max-scrollback 20000)
  (e:variable! vterm-shell "tmux new -A -s emacs")
  (defun e:vterm-input-something ()
    (interactive)
    (let ((input (read-string "input: ")))
      (with-no-warnings (vterm-send-string input)))))

(leaf wakatime-mode
  :if (and (executable-find "wakatime-cli")
           (bound-and-true-p wakatime-api-key))
  :init
  (e:variable! wakatime-cli-path (executable-find "wakatime-cli"))
  (spacemacs/defer-until-after-user-config 'global-wakatime-mode))

(leaf which-key
  :defer-config
  (spacemacs|diminish which-key-mode)
  (e:variable! which-key-sort-order 'which-key-key-order-alpha))

(leaf whitespace
  :hook ((find-file-hook . spacemacs/toggle-whitespace-on)
         (prog-mode-hook . spacemacs/toggle-whitespace-on))
  :config
  (spacemacs|diminish whitespace-mode)
  (e:variable! whitespace-style '(face
                                  trailing
                                  tabs
                                  tab-mark
                                  spaces
                                  space-mark
                                  newline
                                  newline-mark))
  (e:variable! whitespace-space-regexp "\\(\u3000+\\)")
  (e:variable! whitespace-display-mappings '((space-mark   ?\u3000 [?\u30ed])
                                             (tab-mark     ?\t     [?\t])
                                             (newline-mark ?\n     [?\u0024 ?\n])))
  (let ((color "#595D63"))
    (set-face-attribute 'whitespace-trailing nil :background "#800000")
    (set-face-attribute 'whitespace-tab      nil :foreground color :strike-through t)
    (set-face-attribute 'whitespace-space    nil :foreground color)
    (set-face-attribute 'whitespace-newline  nil :foreground color)))

(leaf yarn
  :commands (yarn-install
             yarn-self-update
             yarn-update
             yarn-upgrade)
  :defer-config
  (push '(yarn-compilation-mode :dedicated t :stick t :noselect t) popwin:special-display-config))

(leaf yasnippet
  :defer-config
  (spacemacs|diminish yas-minor-mode))



(require 'user-config-lsp)
(require 'user-config-js)
(require 'user-config-ruby)



(provide 'user-config)

;;; user-config.el ends here
