;;; user-config.el

(eval-and-compile
  (require 'leaf)
  (require 'komunan-lisp-library)
  (require 'user-macro))



(leaf command-logger
  :disabled t
  :config
  (command-logger-on))

(leaf custom-headerline
  :require t
  :config
  (custom-headerline-start))



(leaf japanese-language-environment
  :config
  ;; 言語環境を日本語に設定
  (set-language-environment "Japanese")
  ;; エンコーディングを設定
  (let ((coding-system 'utf-8))
    (prefer-coding-system          coding-system)
    (set-buffer-file-coding-system coding-system))
  ;; ロケールを設定
  (let ((value "ja_JP.UTF-8"))
    (setenv "LANG" value)
    (setenv "LC_ALL" value)))

(leaf spacemacs
  :config
  ;; ダンプ処理に小細工を仕掛けていろいろ上手く調整
  (leaf core-dumper
    :defer-config
    (define-advice spacemacs/dump-emacs (:around (fn &rest args) trick)
      (let ((spacemacs-start-directory user-emacs-directory))
        (apply fn args))))
  ;; モードラインに不要な表示をしない
  (spacemacs/defer-until-after-user-config #'spaceline-toggle-purpose-off)
  (spacemacs/defer-until-after-user-config #'spaceline-toggle-selection-info-off)
  (spacemacs/defer-until-after-user-config #'spaceline-toggle-version-control-off)
  ;; モードラインの表示を改善
  (set-variable 'spaceline-org-clock-p t)
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

(leaf custom-aliases
  :config
  (defalias 'exit 'save-buffers-kill-terminal)
  (defalias 'yes-or-no-p 'y-or-n-p))

(leaf custom-keybindings
  :config
  (spacemacs/set-leader-keys
    "%" 'query-replace
    "&" 'async-shell-command
    "^" 'ace-window
    "|" 'shell-command-on-region
    "gvh" 'vc-region-history)
  (bind-keys*
   :map global-map
   ("C-;" . spacemacs/default-pop-shell)
   ("C-<" . evil-jump-backward)
   ("C->" . evil-jump-forward)
   ("C-^" . ace-window)
   :map ctl-x-map
   ("C-c" . spacemacs/helm-M-x-fuzzy-matching)))



(leaf buffer.c
  :config
  (setq-default bidi-display-reordering nil)
  (setq-default truncate-lines t))

(leaf callproc.c
  :config
  (set-variable 'shell-file-name
                (or (executable-find "zsh")
                    (executable-find "bash")
                    (executable-find "sh"))))

(leaf emacs.c
  :config
  (set-variable 'system-time-locale "C"))

(leaf fileio.c
  :config
  (set-variable 'delete-by-moving-to-trash nil))

(leaf filelock.c
  :config
  (set-variable 'create-lockfiles nil))

(leaf indent.c
  :config
  (spacemacs|add-toggle indent-tabs-mode
    :status indent-tabs-mode
    :on  (setq indent-tabs-mode t)
    :off (setq indent-tabs-mode nil)
    :evil-leader "tT"))

(leaf minibuf.c
  :config
  (set-variable 'history-delete-duplicates t))

(leaf xdisp.c
  :config
  (set-variable 'truncate-partial-width-windows nil))



(leaf configurations-for-Mac
  :if (spacemacs/system-is-mac)
  :config
  ;; タイトルバーの見た目を変更
  (let ((items '((ns-transparent-titlebar . t)
                 (ns-appearance . dark))))
    (dolist (item items)
      (assq-delete-all (car item) initial-frame-alist)
      (assq-delete-all (car item) default-frame-alist)
      (add-to-list 'initial-frame-alist item)
      (add-to-list 'default-frame-alist item)))
  ;; 特殊キーの設定
  (set-variable 'ns-command-modifier 'meta)
  (set-variable 'ns-right-command-modifier 'super)
  (set-variable 'ns-alternate-modifier 'none)
  ;; ちょっと行間を広げる
  (setq-default line-spacing 2))

(leaf configurations-for-WSL1/2
  :config
  ;; WSLの情報を表示
  (when (executable-find "uname")
    (let ((uname (kllib:shell-command-to-string "uname -a")))
      (cond
       ((s-index-of "microsoft-standard" uname)
        (set-variable 'dotspacemacs-frame-title-format "(WSL2) %I@%S"))
       ((s-index-of "Microsoft" uname)
        (set-variable 'dotspacemacs-frame-title-format "(WSL1) %I@%S")))))
  :config
  ;; Windows側のブラウザを起動
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (set-variable 'browse-url-generic-program cmd-exe)
      (set-variable 'browse-url-generic-args    cmd-args))))

(leaf load-local-configurations
  :config
  (let ((private-config (expand-file-name "config" e:private-directory)))
    (condition-case err
        (if (f-exists? private-config)
            (load private-config))
      (error (message "Error: %s" err)))))



(leaf ace-window
  :defer-config
  (set-variable 'aw-keys (number-sequence ?1 ?9))
  (set-variable 'aw-scope 'frame))

(leaf avy
  :defer-config
  (set-variable 'avy-keys (number-sequence ?a ?z))
  (set-variable 'avy-all-windows nil)
  (set-variable 'avy-all-windows-alt t))

(leaf browse-url
  :defer-config
  (defun browse-url-by-choosen (url &optional new-window)
    "選択したブラウザで URL を開く."
    (interactive)
    (let ((browsers '(eww-browse-url browse-url-default-browser)))
      (when browse-url-generic-program
        (add-to-list 'browsers 'browse-url-generic t))
      (funcall (intern (completing-read "Choose Browser: " browsers)) url new-window)))
  (set-variable 'browse-url-browser-function #'browse-url-by-choosen))

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
    :defer-config
    (spacemacs|diminish company-mode)
    (set-variable 'company-transformers
                  '(-distinct
                    company-prescient-transformer
                    company-sort-by-backend-importance)))
  (leaf company-box
    :defer-config
    (spacemacs|diminish company-box-mode)
    (set-variable 'company-box-backends-colors
                  '((company-robe    . (:candidate "#90ee90"))
                    (company-tabnine . (:candidate "#696969"))))))

(leaf *dired
  :config
  (leaf dired
    :bind ((:dired-mode-map
            ("C-c C-e" . wdired-change-to-wdired-mode)))
    :config
    (set-variable 'dired-dwim-target t)
    (set-variable 'dired-listing-switches "-Ahl")
    (set-variable 'dired-omit-files (rx (or (seq bol (? ".") "#")
                                            (seq bol (or "." "..") eol)
                                            (seq bol ".DS_Store" eol))))
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
  :hook ((find-file-hook . spacemacs/toggle-display-line-numbers-mode-on)
         (html-mode-hook . spacemacs/toggle-display-line-numbers-mode-on)
         (org-mode-hook  . spacemacs/toggle-display-line-numbers-mode-on)
         (prog-mode-hook . spacemacs/toggle-display-line-numbers-mode-on))
  :config
  (spacemacs|add-toggle display-line-numbers-mode
    :status display-line-numbers-mode
    :on  (display-line-numbers-mode 1)
    :off (display-line-numbers-mode 0))
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
  (set-variable 'epg-pinentry-mode 'loopback))

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
            ("S" . evil-avy-goto-word-0))
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

(leaf eww
  :bind (:eww-mode-map
         ("e" . eww-open-current-url-with-default-browser))
  :defun (eww-current-url)
  :config
  (set-variable 'eww-search-prefix "https://www.google.com/search?q=")
  (defun eww-open-current-url-with-default-browser ()
    (interactive)
    (browse-url-default-browser (eww-current-url))))

(leaf files
  :defer-config
  (set-variable 'mode-require-final-newline nil)
  (set-variable 'require-final-newline nil))

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

(leaf *google-translate
  :config
  (leaf google-translate-default-ui
    :defer-config
    (set-variable 'google-translate-default-source-language "en")
    (set-variable 'google-translate-default-target-language "ja"))
  (leaf google-translate-core
    :defer-config
    ;; https://github.com/atykhonov/google-translate/issues/52
    ;; https://github.com/atykhonov/google-translate/issues/137
    (defun google-translate--search-tkk ()
      "Search TKK."
      (list 430675 2721866130))))

(leaf *helm
  :config
  (leaf helm
    :bind (([remap eval-expression] . helm-eval-expression-with-eldoc))
    :config
    (set-variable 'helm-buffer-max-length nil))
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

(leaf key-chord
  :init
  (spacemacs/defer-until-after-user-config 'key-chord-mode)
  :config
  (key-chord-define-global " 1" "!")
  (key-chord-define-global " 2" "\"")
  (key-chord-define-global " 3" "#")
  (key-chord-define-global " 4" "$")
  (key-chord-define-global " 5" "%")
  (key-chord-define-global " 6" "&")
  (key-chord-define-global " 7" "'")
  (key-chord-define-global " 8" "(")
  (key-chord-define-global " 9" ")"))

(leaf *magit
  :config
  (leaf magit
    :defer-config
    (set-variable 'magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 15))
    (set-variable 'magit-delete-by-moving-to-trash nil)
    (set-variable 'magit-diff-refine-hunk 'all)
    (set-variable 'magit-diff-refine-ignore-whitespace t)
    (set-variable 'smerge-refine-ignore-whitespace nil)
    (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-modules-overview 'magit-insert-status-headers t)
    (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-skip-worktree-files 'magit-insert-stashes t)
    (evil-define-key 'normal magit-mode-map (kbd "<escape>") 'ignore))
  (leaf magit
    :if (executable-find "ghq")
    :defer-config
    (define-advice magit-repos-alist (:override (&rest _) override)
      (magit-list-repos-uniquify
       (--map (cons (f-short it) it)
              (magit-list-repos))))
    (when (executable-find "ghq")
      (set-variable 'magit-repository-directories
                    (->> (kllib:shell-command-to-list "ghq root --all")
                         (--map (cons it 5))))))
  (leaf transient
    :defer-config
    (set-variable 'transient-default-level 7)))

(leaf markdown-mode
  :defer-config
  (set-variable 'markdown-command "pandoc")
  (remove-hook 'markdown-mode-hook #'orgtbl-mode))

(leaf open-junk-file
  :defer-config
  (set-variable 'open-junk-file-format (expand-file-name "junk/%Y/%Y%m%d-%H%M%S." e:private-directory)))

(leaf org
  :bind* (("C-:" . popwin:daily-report))
  :defvar (org-agenda-file-regexp org-babel-load-languages)
  :config
  (set-variable 'org-agenda-entry-text-leaders (s-concat (s-repeat 25 " ") "│ "))
  (set-variable 'org-agenda-entry-text-maxlines 20)
  (set-variable 'org-directory (expand-file-name "org/" e:private-directory))
  (set-variable 'org-edit-src-content-indentation 0)
  (set-variable 'org-indent-indentation-per-level 2)
  (set-variable 'org-indent-mode-turns-on-hiding-stars nil)
  (set-variable 'org-src-window-setup 'split-window-below)
  (set-variable 'org-startup-folded nil)
  (set-variable 'org-startup-indented t)
  (set-variable 'org-tags-column -110)
  (set-variable 'org-todo-keyword-faces '(("TODO" . org-warning) ("WAITING" . org-done) ("HOLD" . org-done)))
  (set-variable 'org-todo-keywords
                '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                  (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)")))
  (set-variable 'org-capture-templates
                `(("t" "TODO" entry
                   (file+headline org-support/daily-file "INBOX")
                   (file "template/todo.org")
                   :prepend t :jump-to-captured t)
                  ("m" "雑談" entry
                   (file+olp org-support/daily-file "EVENT" "SINGLE")
                   (file "template/meet.org")
                   :prepend t :jump-to-captured t :clock-in t :clock-resume t)
                  ))
  (set-face-attribute 'org-done nil :foreground "#696969")
  (set-face-attribute 'org-todo nil :foreground "#00ff00")
  (set-face-attribute 'org-headline-done nil :foreground "#696969")
  (set-face-attribute 'org-headline-todo nil :foreground "#00ff00")
  (set-face-attribute 'org-level-1 nil :height 1.0)
  (set-face-attribute 'org-level-2 nil :height 1.0)
  (set-face-attribute 'org-level-3 nil :height 1.0)
  (when (f-directory? org-directory)
    (set-variable 'org-default-notes-file (f-expand "index.org" org-directory))
    (set-variable 'org-agenda-files `(,org-directory
                                      ,(f-expand "daily" org-directory)))
    (set-variable 'org-refile-targets '((org-agenda-files :maxlevel . 3))))
  ;; 日報用(暫定)
  (defun popwin:daily-report ()
    (interactive)
    (popwin:popup-buffer (find-file-noselect (org-support/daily-file)) :height 30 :dedicated t :stick t)))

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
  (leaf skk
    :if (executable-find "google-ime-skk")
    :config
    (set-variable 'skk-server-prog (executable-find "google-ime-skk"))
    (set-variable 'skk-server-inhibit-startup-server t)
    (set-variable 'skk-server-host "127.0.0.1")
    (set-variable 'skk-server-portnum 55100)
    (when (require 'prodigy nil t)
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
      (spacemacs/defer-until-after-user-config  'e:prodigy:google-ime-skk))))

(leaf paradox-github
  :defer-config
  (set-variable 'paradox-column-width-package 30)
  (set-variable 'paradox-column-width-version 13)
  (set-variable 'paradox-column-width-star 5)
  (set-variable 'paradox-github-token (getenv "EMACS_PARADOX_GITHUB_TOKEN")))

(leaf password-cache
  :defer-config
  (set-variable 'password-cache-expiry 3600))

(leaf prodigy
  :commands (e:prodigy-start-service)
  :config
  (defun e:prodigy-start-service (name)
    (let ((service (prodigy-find-service name)))
      (when service
        (prodigy-start-service service)))))

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

(leaf popwin
  :defer-config
  (push '(helpful-mode :dedicated t :stick t)             popwin:special-display-config)
  (push '("*Warnings*" :dedicated t :stick t :noselect t) popwin:special-display-config))

(leaf shell-pop
  :defer-config
  (set-variable 'shell-pop-autocd-to-working-dir nil))

(leaf shr
  :defer-config
  (set-variable 'shr-use-fonts nil)
  (set-variable 'shr-use-colors nil)
  (set-variable 'shr-max-image-proportion 0.6))

(leaf simple
  :defer-config
  (set-variable 'set-mark-command-repeat-pop t))

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
    (setq recentf-list
          (->> recentf-list
            (-map 'f-short)
            (-distinct)
            (--filter (or (file-remote-p it)
                          (f-exists? it)))
            (--filter (recentf-include-p it))))))

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

(leaf *vterm
  :config
  (leaf vterm
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
    (set-variable 'vterm-max-scrollback 20000)
    (set-variable 'vterm-shell "tmux new -A -s emacs"))
  (leaf vterm-theme
    :after vterm
    :commands (vterm-theme-solarized-dark
               vterm-theme-onehalf-dark)
    :config
    (vterm-theme-solarized-dark)))

(leaf web-mode
  :defer-config
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset    2)
  (setq-default web-mode-code-indent-offset   2)
  (setq-default web-mode-attr-indent-offset   2))

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
  (require 'yas-rails-helper)
  (spacemacs|diminish yas-minor-mode))



(require 'user-config-lsp)
(require 'user-config-js)
(require 'user-config-php)
(require 'user-config-ruby)



(provide 'user-config)

;;; user-config.el ends here
