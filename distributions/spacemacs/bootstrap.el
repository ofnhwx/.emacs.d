;;; spacemacs/bootstrap.el
;;; Commentary:
;;; Code:

(define-advice dotspacemacs/layers (:after (&rest _) custom)
  "Customize `dotspacemacs/layers'."
  (setq dotspacemacs-configuration-layers
        '(
          ;; +checkers
          (syntax-checking :variables syntax-checking-enable-tooltips nil)
          ;; +completion
          auto-completion
          helm
          ;; +emacs
          org
          ;; +filetree
          treemacs
          ;; +frameworks
          react
          ruby-on-rails
          vue
          ;; +fun
          emoji
          ;; +intl
          japanese
          ;; +lang
          (ruby :variables ruby-test-runner 'rspec)
          csv
          emacs-lisp
          graphql
          html
          javascript
          kotlin
          markdown
          rust
          shell-scripts
          sql
          swift
          typescript
          vimscript
          yaml
          ;; +misc
          copy-as-format
          dtrt-indent
          multiple-cursors
          ;; +readers
          pdf
          ;; +source-control
          (git :variables git-magit-status-fullscreen t)
          (version-control :variables version-control-diff-side 'left)
          ;; +spacemacs
          (spacemacs-completion :variables helm-use-fuzzy nil)
          ;; +tags
          gtags
          ;; +themes
          colors
          ;; +tools
          (lsp :variables lsp-ui-sideline-enable nil)
          (shell :variables shell-default-shell 'vterm)
          dap
          docker
          nginx
          prodigy
          restclient
          systemd
          tree-sitter
          vagrant
          ;; +web
          eww
          ;; +web-services
          search-engine
          ;; +private
          completions
          misc
          ))
  (setq dotspacemacs-additional-packages
        '(
          (helm-fzf             :location (recipe :fetcher github :repo "ofnhwx/helm-fzf"))
          (komunan-lisp-library :location (recipe :fetcher github :repo "ofnhwx/komunan-lisp-library"))
          (locale-eaw           :location (recipe :fetcher github :repo "hamano/locale-eaw"))
          (ls-lisp-extension    :location (recipe :fetcher github :repo "ofnhwx/ls-lisp-extension"))
          (yarn                 :location (recipe :fetcher github :repo "jmfirth/yarn.el"))
          ))
  (setq dotspacemacs-excluded-packages
        '(
          ;; +spacemacs/spacemacs-editing
          dired-quick-sort
          ;; +spacemacs/spacemacs-evil
          evil-escape
          ;; +completion/helm
          helm-flx
          ;; +intl/japanese
          pangu-spacing
          )))

(define-advice dotspacemacs/init (:after (&rest _) custom)
  "Customize `dotspacemacs/init'."
  (cond
   ((eq system-type 'darwin)
    (setq dotspacemacs-default-font '("Cica" :size 16.0)))
   (t
    (setq dotspacemacs-default-font '("Cica" :size 12.0))))
  (setq dotspacemacs-ask-for-lazy-installation t)
  (setq dotspacemacs-editing-style 'hybrid)
  (setq dotspacemacs-enable-paste-transient-state t)
  (setq dotspacemacs-enable-server t)
  (setq dotspacemacs-highlight-delimiters 'any)
  (setq dotspacemacs-large-file-size 10)
  (setq dotspacemacs-loading-progress-bar nil)
  (setq dotspacemacs-maximized-at-startup nil)
  (setq dotspacemacs-mode-line-theme '(spacemacs :separator bar))
  (setq dotspacemacs-new-empty-buffer-major-mode 'fundamental-mode)
  (setq dotspacemacs-scratch-buffer-persistent t)
  (setq dotspacemacs-scratch-mode 'lisp-interaction-mode)
  (setq dotspacemacs-scroll-bar-while-scrolling nil)
  (setq dotspacemacs-startup-lists nil)
  (setq dotspacemacs-themes '(modus-vivendi modus-operandi))
  (setq dotspacemacs-use-SPC-as-y t))

(define-advice dotspacemacs/user-init (:after (&rest _) custom)
  "Customize `dotspacemacs/user-init'."
  (prog1 "spacemacs より先に設定しておきたいもの"
    ;; ssh-agent
    (let ((sock (expand-file-name "gcr/ssh" (getenv "XDG_RUNTIME_DIR"))))
      (when (file-exists-p sock)
        (setenv "SSH_AUTH_SOCK" sock)))
    ;; +distributions/spacemacs-bootstrap
    (set-variable 'vim-style-remap-Y-to-y$ t)
    (set-variable 'which-key-enable-extended-define-key t)
    (set-variable 'which-key-show-early-on-C-h t)
    ;; recentf
    (when (require 'recentf)
      (set-variable 'recentf-save-file (expand-file-name "recentf" spacemacs-cache-directory))
      (set-variable 'recentf-max-saved-items 3000)
      (recentf-mode 1))
    ;; others
    (set-variable 'custom-file (make-temp-file "emacs-custom-" nil ".el"))
    (set-variable 'package-gnupghome-dir (expand-file-name "gnupg" spacemacs-cache-directory))
    (set-variable 'spacemacs-env-vars-file (expand-file-name "spacemacs.env" spacemacs-cache-directory))
    (set-variable 'viper-mode nil)))

(define-advice dotspacemacs/user-config (:after (&rest _) custom)
  "Customize `dotspacemacs/user-config'."
  (load (expand-file-name "lisp/user-config" e:distribution-directory)))



(define-advice spacemacs/recompile-elpa (:after (&rest _) recompile-distribution)
  "elpa パッケージのコンパイルに引っかけて設定をリコンパイル."
  (byte-recompile-directory e:distribution-directory))

(define-advice spacemacs/set-default-font (:after (&rest _) japanese-font-setting)
  "spacemacs のフォント設定に引っかけて日本語関連の設定を行う."
  (let ((font (car dotspacemacs-default-font)))
    (set-fontset-font t 'unicode (font-spec :family font))
    (set-variable 'face-font-rescale-alist (list font 1.00))
    (when (fboundp 'eaw-fullwidth)
      (eaw-fullwidth))))



(defun e:load-early-init ()
  (let ((user-emacs-directory (expand-file-name "spacemacs/" e:external-directory)))
    (setenv "SPACEMACSDIR" e:distribution-directory)
    (load (expand-file-name "early-init" user-emacs-directory))))

(defun e:load-init ()
  (let ((user-emacs-directory (expand-file-name "spacemacs/" e:external-directory)))
    (setenv "SPACEMACSDIR" e:distribution-directory)
    (load (expand-file-name "init" user-emacs-directory))))

(defun e:load-dump-init ()
  (let ((user-emacs-directory (expand-file-name "spacemacs/" e:external-directory)))
    (setenv "SPACEMACSDIR" e:distribution-directory)
    (load (expand-file-name "dump-init" user-emacs-directory))))

;;; spacemacs/bootstrap.el ends here
