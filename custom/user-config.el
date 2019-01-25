
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
