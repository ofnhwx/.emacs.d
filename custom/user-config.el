
(progn
  (defun e:setup-font ()
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
    (let* ((fontname "Ricty Diminished Discord")
           (size 14)
           (font (font-spec :family fontname :size size))
           (height (* size 10)))
      (set-fontset-font t 'japanese-jisx0208 font)
      (set-fontset-font t 'katakana-jisx0201 font)
      (set-face-attribute 'default nil :family fontname :height height))
    ;; 対策: East Asian Ambiguous Width
    (add-to-list 'load-path (expand-file-name "locale-eaw" e:lisp-directory))
    (when (require 'eaw nil t)
      (eaw-fullwidth))
    ;;
    t)
  ;; いろいろあったのでこんな感じで
  (add-hook 'window-setup-hook 'e:setup-font))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package core-dotspacemacs
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

(use-package ace-window
  :config
  (bind-keys
   :map global-map
   ("C-^" . ace-window)))

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

(use-package display-line-numbers
  :config
  (when (fboundp 'display-line-numbers-mode)
    (defun display-line-numbers-mode-on ()
      "`display-line-numbers-mode'を有効化."
      (interactive)
      (display-line-numbers-mode 1))
    (defun display-line-numbers-mode-off ()
      "`display-line-numbers-mode'を無効化."
      (interactive)
      (display-line-numbers-mode 0))
    (add-hook 'prog-mode-hook 'display-line-numbers-mode-on)
    (add-hook 'find-file-hook 'display-line-numbers-mode-on)
    (add-hook 'html-mode-hook 'display-line-numbers-mode-on)
    (setq-default display-line-numbers-width 4)))

(use-package evil
  :config
  ;; variables
  (progn
    (setq evil-cross-lines t)
    (setq evil-insert-state-map (make-sparse-keymap))
    (setq evil-move-cursor-back nil))
  ;; advices
  (progn
    (defun e:evil-force-normal-state ()
      (when (member evil-state '(insert hybrid visual))
        (evil-force-normal-state)))
    (defun e:save-buffer:after (&rest args)
      (e:evil-force-normal-state))
    (defun e:keyboard-quit:before (&rest args)
      (e:evil-force-normal-state))
    (advice-add 'save-buffer :after 'e:save-buffer:after)
    (advice-add 'keyboard-quit :before 'e:keyboard-quit:before))
  ;; keybindings
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
   ("gj" . evil-next-line)
   ("gk" . evil-previous-line)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired
  :defer t
  :config
  (when (require 'ls-lisp nil t)
    (setq ls-lisp-use-insert-directory-program nil)
    (setq ls-lisp-dirs-first t))
  (setq dired-use-ls-dired t)
  (setq dired-listing-switches "-ahl")
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top))

(use-package eww
  :defer t
  :config
  (defvar e:eww-enable-colorize nil)
  (defun e:eww-colorize-region:around (&rest args)
    (when e:eww-enable-colorize
      (apply (car args) (cdr args))))
  (defun e:eww-colorize-on ()
    (interactive)
    (setq-local e:eww-enable-colorize t)
    (eww-reload))
  (defun e:eww-colorize-off ()
    (interactive)
    (setq-local e:eww-enable-colorize nil)
    (eww-reload))
  (advice-add 'eww-colorize-region :around 'e:eww-colorize-region:around)
  (advice-add 'shr-colorize-region :around 'e:eww-colorize-region:around))

(use-package flycheck
  :defer t
  :config
  (setq flycheck-idle-buffer-switch-delay 3.0)
  (setq flycheck-idle-change-delay 3.0))

(use-package magit
  :defer t
  :config
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 15))
  (setq magit-diff-refine-hunk 'all)
  (setq smerge-refine-ignore-whitespace nil)
  (magit-define-popup-switch 'magit-log-popup ?l "Always sort by date" "--date-order"))

(use-package navi2ch
  :load-path "lisp/navi2ch"
  :defer t
  :commands (navi2ch)
  :init
  (setq navi2ch-net-http-proxy "127.0.0.1:9080")
  :config
  (when (require 'prodigy nil t)
    (let ((cmd (expand-file-name "2chproxy.pl/2chproxy.pl" e:util-directory))
          (yml (expand-file-name "2chproxy.yml" e:custom-directory)))
      (when (and (executable-find cmd)
                 (file-exists-p yml))
        (prodigy-define-service
          :name "2chproxy.pl"
          :command (format "%s --config %s" cmd yml)
          :tags '(general)
          :kill-signal 'sigkill)))
    (defun e:prodigy:2chproxy.pl ()
      (interactive)
      (e:prodigy-start-service "2chproxy.pl"))
    (e:prodigy:2chproxy.pl)))

(use-package notmuch
  :defer t
  :config
  (setq notmuch-archive-tags '("-inbox" "-unread"))
  (setq notmuch-column-control 1.0)
  (setq notmuch-hello-thousands-separator ",")
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-show-empty-saved-searches t)
  (setq notmuch-show-logo nil)
  (setq notmuch-hello-hide-tags
        '("encrypted" "drafts" "flagged" "inbox" "sent" "signed" "spam" "unread"))
  (setq notmuch-saved-searches
        '((:name "受信トレイ" :query "tag:inbox"   :key "i")
          (:name "未読　　　" :query "tag:unread"  :key "u")
          (:name "スター付き" :query "tag:flagged" :key "f")
          (:name "送信済み　" :query "tag:sent"    :key "t")
          (:name "下書き　　" :query "tag:draft"   :key "d")
          (:name "すべて　　" :query "*"           :key "a")
          (:name "迷惑メール" :query "tag:spam"    :key "s")))
  (setenv "XAPIAN_CJK_NGRAM" "1"))

(use-package pangu-spacing
  :defer t
  :config
  (setq pangu-spacing-real-insert-separtor nil))

(use-package prodigy
  :defer t
  :config
  (defun e:prodigy-start-service (name)
    (let ((service (prodigy-find-service name)))
      (when service
        (prodigy-start-service service)))))

(use-package skk
  :defer t
  :config
  (when (and (executable-find "google-ime-skk")
             (require 'prodigy nil t))
    (prodigy-define-service
      :name "google-ime-skk"
      :command "google-ime-skk"
      :tags '(general)
      :kill-signal 'sigkill)
    (defun e:prodigy:google-ime-skk ()
      (interactive)
      (e:prodigy-start-service "google-ime-skk"))
    (e:prodigy:google-ime-skk))
  (defun e:skk-latin-mode-on:before (&rest args)
    (unless skk-mode-invoked
      (skk-mode-invoke)))
  (require 'skk-study nil t)
  (advice-add 'skk-latin-mode-on :before 'e:skk-latin-mode-on:before)
  (add-hook 'evil-hybrid-state-entry-hook 'skk-latin-mode-on)
  (add-hook 'evil-hybrid-state-exit-hook  'skk-mode-exit))

(use-package which-key
  :defer t
  :config
  (setq which-key-idle-delay 1.0)
  (setq which-key-idle-secondary-delay 0.1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vagrant-tramp
  :defer t
  :config
  (let ((version (pkg-info-package-version "vagrant-tramp"))
        (target '(20160427 2332)))
    (if (equal version target)
        (defun vagrant-tramp--all-boxes ()
          "List of VMs per `vagrant global-status` as alists."
          (let* ((status-cmd "vagrant global-status --machine-readable")
                 (status-raw (shell-command-to-string status-cmd))
                 (status-lines (-drop 7 (split-string status-raw "\n")))
                 (status-data-raw (--map (mapconcat 'identity
                                                    (-drop 4 (split-string it ",")) ",")
                                         status-lines))
                 (status-data (--map (replace-regexp-in-string " " "" it) status-data-raw))
                 (status-groups (-butlast (-split-on "" status-data)))
                 (vm-attrs '(id name provider state dir)))
            (--map (-zip vm-attrs it) status-groups)))
      (spacemacs-buffer/warning "`vagrant-tramp' was updated."))))
