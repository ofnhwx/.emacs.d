(eval-and-compile
  (require 'leaf))

(leaf japanese-environment
  :config
  (leaf language-environment
    :config
    (set-language-environment "Japanese"))
  (leaf encoding
    :config
    (let ((coding-system 'utf-8))
      (prefer-coding-system          coding-system)
      (set-default-coding-systems    coding-system)
      (set-buffer-file-coding-system coding-system)
      (set-terminal-coding-system    coding-system)
      (set-keyboard-coding-system    coding-system)))
  (leaf locale
    :config
    (let ((value "ja_JP.UTF-8"))
      (setenv "LANG" value)
      (setenv "LC_ALL" value))))

(leaf general
  :config
  (leaf e:place-in-cache
    :config
    (defmacro e:place-in-cache (variable path)
      `(set-variable ',variable (expand-file-name ,path spacemacs-cache-directory))))
  (leaf advice-auto-reset-mode-line-colors
    :doc "テスト成否によるモードラインの色の変更を一定時間で戻す"
    :config
    (defvar e:mode-line-foreground (face-foreground 'mode-line))
    (defvar e:mode-line-background (face-background 'mode-line))
    (define-advice set-face-attribute (:around (fn &rest args) auto-reset-mode-line-colors)
      (apply fn args)
      (when (eq (car args) 'mode-line)
        (let ((inhibit-quit t))
          (sit-for 3)
          (funcall fn 'mode-line nil
                   :foreground e:mode-line-foreground
                   :background e:mode-line-background))))))

(leaf ace-window
  :bind (("C-^" . ace-window))
  :config
  (set-variable 'aw-keys (number-sequence ?1 ?9))
  (set-variable 'aw-scope 'frame))

(leaf atomic-chrome
  :config
  (spacemacs/defer-until-after-user-config #'atomic-chrome-start-server))

(leaf avy
  :config
  (set-variable 'avy-keys (number-sequence ?a ?z))
  (set-variable 'avy-all-windows nil)
  (set-variable 'avy-all-windows-alt t))

(leaf codic
  :after codic
  :config
  (set-variable 'codic-api-token (e:auth-source-get 'token :host "codic")))

(leaf evil
  :config
  (set-variable 'evil-cross-lines t)
  (set-variable 'evil-move-cursor-back nil)
  (leaf evil-keybind
    :config
    (setq evil-disable-insert-state-bindings t)
    (bind-keys
     ;; モーションモード(motion -> normal -> visual)
     :map evil-motion-state-map
     ("C-^" . nil) ;; evil-buffer
     ;; 通常モード
     :map evil-normal-state-map
     ("<down>" . evil-next-visual-line)
     ("<up>" . evil-previous-visual-line)
     ("j" . evil-next-visual-line)
     ("k" . evil-previous-visual-line)
     ("gj" . evil-avy-goto-line-below)
     ("gk" . evil-avy-goto-line-above)
     ("S" . evil-avy-goto-char-timer)
     ;; ビジュアルモード
     :map evil-visual-state-map
     ;; 挿入モード
     :map evil-insert-state-map
     ;; オペレーターモード
     :map evil-operator-state-map
     ;; 置き換えモード
     :map evil-replace-state-map
     ;; Emacsモード
     :map evil-emacs-state-map))
  (leaf evil-easymotion
    :config
    (leaf e:evil-em-command
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
  (leaf evil-little-word
    :config
    (ignore-errors
      (require 'evil-little-word nil t)))
  (leaf evil-textobj-between
    :require t)
  (leaf evil-owl
    :config
    (evil-owl-mode 1))
  (leaf e:evil-force-normal-state
    :config
    (defun e:evil-force-normal-state ()
      (cond
       ((eq evil-state 'visual)
        (evil-exit-visual-state))
       ((member evil-state '(insert hybrid))
        (evil-force-normal-state)))))
  (leaf evil-advice
    :doc "保存時等にノーマルステートに戻す"
    :config
    (define-advice save-buffer (:after (&rest _) evil-force-normal-state)
      (e:evil-force-normal-state))
    (define-advice keyboard-quit (:before (&rest _) evil-force-normal-state)
      (e:evil-force-normal-state))))

(leaf helm
  :config
  (leaf helm-tramp
    :config
    (leaf helm-tramp-advice
      :after helm-tramp
      :require tramp
      :doc "ssh の設定ファイルから候補を追加"
      :config
      (eval-when-compile (require 'helm-tramp))
      (define-advice helm-tramp--candidates (:filter-return (result) add-candidates-from-ssh-config)
        (let ((items (->> (tramp-get-completion-function "ssh")
                          (-map #'eval)
                          (-flatten)
                          (--filter (not (string-equal it tramp-default-host)))
                          (--map (list (format "/%s:%s:" tramp-default-method it)
                                       (format "/ssh:%s|sudo:%s:/" it it)))
                          (-flatten))))
          (-distinct (-union result items)))))))

(leaf leaf
  :config
  (leaf leaf-tree
    :config
    (set-variable 'imenu-list-size 30)
    (set-variable 'imenu-list-position 'left)))

(leaf lang
  :config
  (leaf php
    :config
    (spacemacs|add-company-backends :modes php-mode))
  (leaf ruby
    :config
    (set-variable 'ruby-insert-encoding-magic-comment nil)
    (leaf rubocopfmt
      :config
      (set-variable 'rubocopfmt-use-bundler-when-possible nil)))
  (leaf lsp
    :config
    (leaf lsp-mode
      :config
      (e:place-in-cache lsp-session-file ".lsp-session-v1"))
    (leaf lsp-java
      :config
      (e:place-in-cache lsp-java-server-install-dir "java/lsp")
      (e:place-in-cache lsp-java-workspace-dir "java/workspace"))))

(leaf magit
  :config
  (leaf transient
    :config
    (set-variable 'transient-default-level 7)))

(leaf migemo
  :if e:enable-cmigemo-p
  :config
  (leaf avy-migemo
    :config
    (leaf avy-migemo-patch
      :require pkg-info
      :doc "引数の数が変更されているため暫定的にここで対応"
      :config
      (let ((version (pkg-info-format-version (pkg-info-package-version 'avy-migemo)))
            (target "20180716.1455"))
        (if (string-equal version target)
            (with-eval-after-load 'avy-migemo
              (define-advice avy--generic-jump (:filter-args (args) patch-20180716-1455)
                (if (= (length args) 4)
                    args
                  (e:remove-nth 2 args))))
          (spacemacs-buffer/warning "`avy-migemo' was updated."))))))

(leaf notmuch
  :if e:enable-notmuch-p
  :config
  (leaf notmuch-advice
    :after notmuch
    :doc "終了時にレイアウトを削除"
    :config
    (eval-when-compile 'notmuch-lib)
    (define-advice notmuch-bury-or-kill-this-buffer (:around (fn) kill-layout)
      (let ((kill (eq (e:major-mode) 'notmuch-hello-mode)))
        (prog1
            (funcall fn)
          (if kill
              (persp-kill notmuch-spacemacs-layout-name)))))))

(leaf org
  :config
  (set-variable 'org-directory (expand-file-name "org/" e:private-directory))
  (when (f-directory? org-directory)
    (set-variable 'org-default-notes-file (expand-file-name "notes.org" org-directory))
    (set-variable 'org-agenda-files (-union (list org-default-notes-file)
                                            (directory-files-recursively org-directory org-agenda-file-regexp)))
    (set-variable 'org-refile-targets '((org-agenda-files :maxlevel . 3))))
  (set-variable 'org-todo-keywords
                '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                  (sequence "WAITING(w@)" "HOLD(h@)" "|" "CANCELLED(c@)")))
  (set-variable 'org-edit-src-content-indentation 0))

(leaf skk
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
    (set-variable 'skk-use-jisx0201-input-method t))
  (leaf e:skk-mode
    :config
    (defun e:skk-mode ()
      "skk の有効化で半角英数入力にする"
      (interactive)
      (if (bound-and-true-p skk-mode)
          (skk-latin-mode-on)
        (let ((skk-mode-hook (-union skk-mode-hook '(skk-latin-mode-on))))
          (skk-mode)))))
  (leaf e:prodigy:google-ime-skk
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
        (e:prodigy-start-service service))))
  (leaf google-ime-skk
    :if (executable-find "google-ime-skk")
    :require prodigy
    :doc "設定"
    :config
    (set-variable 'skk-server-prog (executable-find "google-ime-skk"))
    (set-variable 'skk-server-inhibit-startup-server t)
    (set-variable 'skk-server-host "127.0.0.1")
    (set-variable 'skk-server-portnum 55100)
    :doc "起動"
    :config
    (spacemacs/defer-until-after-user-config  #'e:prodigy:google-ime-skk)))

(leaf prodigy
  :commands (e:prodigy-start-service)
  :config
  (leaf e:prodigy-start-service
    :config
    (defun e:prodigy-start-service (name)
      (let ((service (prodigy-find-service name)))
        (when service
          (prodigy-start-service service))))))

(leaf so-long
  :require t
  :config
  (global-so-long-mode 1))

(leaf recentf
  :config
  (set-variable 'recentf-max-menu-items 20)
  (set-variable 'recentf-max-saved-items 3000)
  (set-variable 'recentf-filename-handlers '(abbreviate-file-name))
  (leaf recentf-advice
    :after recentf
    :doc "存在しないファイルを履歴から削除する"
    :config
    (define-advice recentf-save-list (:before (&rest _) remove-non-existing-files)
      (setq recentf-list
            (->> recentf-list
                 (-map 'f-short)
                 (-distinct)
                 (--filter (or (file-remote-p it)
                               (f-exists? it))))))))

(leaf tramp
  :require t
  :config
  (set-variable 'tramp-default-host "localhost")
  (leaf tramp-sh
    :doc "ssh/conf.d の中身から接続先を追加"
    :config
    (let ((functions (->> (ignore-errors (f-files "~/.ssh/conf.d/hosts" nil t))
                          (--map (list #'tramp-parse-sconfig it)))))
      (--each '("ssh" "scp")
        (let ((new-functions (-union (tramp-get-completion-function it) functions)))
          (tramp-set-completion-function it new-functions))))))

(leaf treemacs
  :config
  (e:place-in-cache treemacs-persist-file "treemacs-persist")
  (e:place-in-cache treemacs-last-error-persist-file "treemacs-persist-at-last-error"))

(leaf url
  :config
  (leaf url-cache
    :config
    (e:place-in-cache url-cache-directory "url/cache"))
  (leaf url-cookie
    :config
    (e:place-in-cache url-cookie-file "url/cookies")))

(leaf visual-regexp
  :bind (([remap query-replace] . vr/query-replace)))

(leaf whitespace
  :hook ((find-file-hook prog-mode-hook) . e:whitespace-mode-on)
  :config
  (leaf whitespace-variables
    :config
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
                    (newline-mark ?\n     [?\u0024 ?\n]))))
  (leaf whitespace-faces
    :config
    (set-face-attribute 'whitespace-trailing nil :background "#800000")
    (let ((color "#595D63"))
      (set-face-attribute 'whitespace-tab      nil :foreground color :strike-through t)
      (set-face-attribute 'whitespace-space    nil :foreground color)
      (set-face-attribute 'whitespace-newline  nil :foreground color)))
  (leaf e:whitespace-mode-on
    :config
    (defun e:whitespace-mode-on ()
      (interactive)
      (whitespace-mode 1))))
