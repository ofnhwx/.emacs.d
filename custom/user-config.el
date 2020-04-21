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

(leaf ace-window
  :bind (("C-^" . ace-window))
  :config
  (set-variable 'aw-keys (number-sequence ?1 ?9))
  (set-variable 'aw-scope 'frame))

(leaf atomic-chrome
  :config
  (atomic-chrome-start-server))

(leaf avy
  :config
  (set-variable 'avy-keys (number-sequence ?a ?z))
  (set-variable 'avy-all-windows nil)
  (set-variable 'avy-all-windows-alt t))

(leaf codic
  :after codic
  :config
  (set-variable 'codic-api-token (e:auth-source-get 'token :host "codic")))

(leaf leaf
  :config
  (leaf leaf-tree
    :config
    (set-variable 'imenu-list-size 30)
    (set-variable 'imenu-list-position 'left)))

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

(leaf so-long
  :require t
  :config
  (global-so-long-mode 1))

(leaf treemacs
  :config
  (set-variable 'treemacs-persist-file (expand-file-name "treemacs-persist" spacemacs-cache-directory))
  (set-variable 'treemacs-last-error-persist-file (expand-file-name "treemacs-persist-at-last-error" spacemacs-cache-directory)))

(leaf url
  :config
  (leaf url-cache
    :config
    (set-variable 'url-cache-directory (expand-file-name "url/cache" spacemacs-cache-directory)))
  (leaf url-cookie
    :config
    (set-variable 'url-cookie-file (expand-file-name "url/cookies" spacemacs-cache-directory))))

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
