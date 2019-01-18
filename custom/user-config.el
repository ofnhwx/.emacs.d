;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 言語環境・フォント
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-environment "Japanese")

(let ((coding-system 'utf-8))
  (prefer-coding-system          coding-system)
  (set-default-coding-systems    coding-system)
  (set-buffer-file-coding-system coding-system)
  (set-terminal-coding-system    coding-system)
  (set-keyboard-coding-system    coding-system))

(let* ((fontname "Ricty Diminished Discord")
       (size 14)
       (font (font-spec :family fontname :size size))
       (height (* size 10)))
  (set-fontset-font t 'japanese-jisx0208 font)
  (set-fontset-font t 'katakana-jisx0201 font)
  (set-face-attribute 'default nil :family fontname :height height))

(progn
  (add-to-list 'load-path (expand-file-name "locale-eaw" e:lisp-directory))
  (when (require 'eaw nil t)
    (defun eaw-fullwidth-interactive ()
      (interactive)
      (eaw-fullwidth)
      t)
    (eaw-fullwidth-interactive)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; モードラインにエンコーディングを表示
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spaceline-define-segment buffer-encoding-abbrev
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (list (replace-regexp-in-string "-with-signature\\|-unix\\|-dos\\|-mac" "" buf-coding)
          (concat (and (string-match "with-signature" buf-coding) "ⓑ")
                  (and (string-match "unix"           buf-coding) "ⓤ")
                  (and (string-match "dos"            buf-coding) "ⓓ")
                  (and (string-match "mac"            buf-coding) "ⓜ")
                  )))
  :separator " ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 行番号の表示
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
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
  (setq-default display-line-numbers-width 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-keys
 :map global-map
 ("C-^" . ace-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil:customize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :config
  (setq evil-cross-lines t)
  (setq evil-insert-state-map (make-sparse-keymap))
  (setq evil-move-cursor-back nil)
  (defun evil-save-and-normal-state ()
    "Save and force normal state."
    (interactive)
    (save-buffer)
    (evil-force-normal-state))
  (defun evil-keyboard-quit ()
    "Keyboard quit and force normal state."
    (interactive)
    (evil-force-normal-state)
    (keyboard-quit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil:keybind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :config
  (setq evil-insert-state-map (make-sparse-keymap))
  (bind-keys
   ;; モーションモード(motion -> normal -> visual)
   :map evil-motion-state-map
   ("C-^" . nil) ;; evil-buffer
   ("C-g" . evil-keyboard-quit)
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
   ("C-g" . evil-keyboard-quit)
   ("C-x C-s" . evil-save-and-normal-state)
   ("<escape>" . evil-force-normal-state)
   ;; オペレーターモード
   :map evil-operator-state-map
   ;; 置き換えモード
   :map evil-replace-state-map
   ;; Emacsモード
   :map evil-emacs-state-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; layer:git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :config
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 15))
  (setq magit-diff-refine-hunk 'all)
  (setq smerge-refine-ignore-whitespace nil)
  (magit-define-popup-switch 'magit-log-popup ?l "Always sort by date" "--date-order"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; layer:japanese
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package skk
  :config
  (add-hook 'evil-insert-state-entry-hook 'skk-latin-mode-on)
  (add-hook 'evil-insert-state-exit-hook  'skk-mode-exit))

(use-package pangu-spacing
  :config
  (setq pangu-spacing-real-insert-separtor nil))
