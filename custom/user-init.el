
(progn
  (progn ;; define
    (defvar e:dotspacemacs-configuration-layers nil)
    (defvar e:dotspacemacs-additional-packages nil)
    (defvar e:dotspacemacs-excluded-packages nil)
    (defvar e:system-type-darwin-p (eq system-type 'darwin))
    (defvar e:enable-cmigemo-p (executable-find "cmigemo"))
    (defvar e:enable-java-p    (executable-find "java"))
    (defvar e:enable-notmuch-p (executable-find "notmuch"))
    (defvar e:enable-vagrant-p (executable-find "vagrant")))
  (progn ;; +distributions/spacemacs-bootstrap
    (set-variable 'vim-style-remap-Y-to-y$ t))
  (progn ;; +spacemacs/spacemacs-completion
    (set-variable 'helm-use-fuzzy nil))
  (progn ;; +checkers
    (add-to-list 'e:dotspacemacs-configuration-layers 'syntax-checking))
  (progn ;; +completion
    (progn ;; auto-completion
      (add-to-list 'e:dotspacemacs-configuration-layers 'auto-completion)
      (set-variable 'auto-completion-enable-help-tooltip t)
      (set-variable 'auto-completion-use-company-box t))
    (add-to-list 'e:dotspacemacs-configuration-layers 'helm))
  (progn ;; +emacs
    (add-to-list 'e:dotspacemacs-configuration-layers 'better-defaults)
    (add-to-list 'e:dotspacemacs-configuration-layers 'org))
  (progn ;; +email
    (when e:enable-notmuch-p
      (add-to-list 'e:dotspacemacs-configuration-layers 'notmuch)))
  (progn ;; +filetree
    (add-to-list 'e:dotspacemacs-configuration-layers 'treemacs))
  (progn ;; +frameworks
    (add-to-list 'e:dotspacemacs-configuration-layers 'react)
    (add-to-list 'e:dotspacemacs-configuration-layers 'ruby-on-rails))
  (progn ;; +fun
    (add-to-list 'e:dotspacemacs-configuration-layers 'emoji))
  (progn ;; +intl
    (progn ;; japanese
      (add-to-list 'e:dotspacemacs-configuration-layers 'japanese)
      (add-to-list 'e:dotspacemacs-excluded-packages 'pangu-spacing)))
  (progn ;; +lang
    (add-to-list 'e:dotspacemacs-configuration-layers 'csharp)
    (add-to-list 'e:dotspacemacs-configuration-layers 'csv)
    (add-to-list 'e:dotspacemacs-configuration-layers 'emacs-lisp)
    (add-to-list 'e:dotspacemacs-configuration-layers 'go)
    (add-to-list 'e:dotspacemacs-configuration-layers 'html)
    (progn ;; java
      (add-to-list 'e:dotspacemacs-configuration-layers 'java)
      (when e:enable-java-p
        (set-variable 'java-backend 'meghanada)))
    (add-to-list 'e:dotspacemacs-configuration-layers 'javascript)
    (add-to-list 'e:dotspacemacs-configuration-layers 'kotlin)
    (add-to-list 'e:dotspacemacs-configuration-layers 'markdown)
    (progn ;; php
      (add-to-list 'e:dotspacemacs-configuration-layers 'php)
      (add-to-list 'e:dotspacemacs-excluded-packages 'company-php))
    (add-to-list 'e:dotspacemacs-configuration-layers 'ruby)
    (add-to-list 'e:dotspacemacs-configuration-layers 'shell-scripts)
    (add-to-list 'e:dotspacemacs-configuration-layers 'sql)
    (add-to-list 'e:dotspacemacs-configuration-layers 'vimscript)
    (add-to-list 'e:dotspacemacs-configuration-layers 'yaml))
  (progn ;; +misc
    (add-to-list 'e:dotspacemacs-configuration-layers 'copy-as-format)
    (add-to-list 'e:dotspacemacs-configuration-layers 'multiple-cursors))
  (progn ;; +os
    (when (eq system-type 'darwin)
      (add-to-list 'e:dotspacemacs-configuration-layers 'osx)))
  (progn ;; +pair-programming)
    (add-to-list 'e:dotspacemacs-configuration-layers 'floobits))
  (progn ;; +readers
    (add-to-list 'e:dotspacemacs-configuration-layers 'elfeed)
    (add-to-list 'e:dotspacemacs-configuration-layers 'dash))
  (progn ;; +source-control
    (progn ;; git
      (add-to-list 'e:dotspacemacs-configuration-layers 'git)
      (set-variable 'git-magit-status-fullscreen t))
    (add-to-list 'e:dotspacemacs-configuration-layers 'github))
  (progn ;; +tags
    (add-to-list 'e:dotspacemacs-configuration-layers 'gtags))
  (progn ;; +tools
    (add-to-list 'e:dotspacemacs-configuration-layers 'docker)
    (add-to-list 'e:dotspacemacs-configuration-layers 'lsp)
    (add-to-list 'e:dotspacemacs-configuration-layers 'nginx)
    (add-to-list 'e:dotspacemacs-configuration-layers 'prodigy)
    (add-to-list 'e:dotspacemacs-configuration-layers 'restclient)
    (progn ;; shell
      (add-to-list 'e:dotspacemacs-configuration-layers 'shell)
      (set-variable 'shell-enable-smart-eshell t)
      (set-variable 'shell-default-shell (cond
                                          ((eq system-type 'windows-nt) 'eshell)
                                          (t 'vterm))))
    (when e:enable-vagrant-p
      (add-to-list 'e:dotspacemacs-configuration-layers 'vagrant)))
  (progn ;; +web-services
    (add-to-list 'e:dotspacemacs-configuration-layers 'search-engine))
  (progn ;; 追加パッケージ
    (progn ;; github
      (add-to-list 'e:dotspacemacs-additional-packages
                   '(evil-plugins :location (recipe :fetcher github :repo "tarao/evil-plugins")))
      (add-to-list 'e:dotspacemacs-additional-packages
                   '(helm-fzf     :location (recipe :fetcher github :repo "ofnhwx/helm-fzf")))
      (add-to-list 'e:dotspacemacs-additional-packages
                   '(locale-eaw   :location (recipe :fetcher github :repo "hamano/locale-eaw"))))
    (add-to-list 'e:dotspacemacs-additional-packages 'atomic-chrome)
    (add-to-list 'e:dotspacemacs-additional-packages 'basic-mode)
    (add-to-list 'e:dotspacemacs-additional-packages 'codic)
    (add-to-list 'e:dotspacemacs-additional-packages 'company-tabnine)
    (add-to-list 'e:dotspacemacs-additional-packages 'company-try-hard)
    (add-to-list 'e:dotspacemacs-additional-packages 'dired-filter)
    (add-to-list 'e:dotspacemacs-additional-packages 'elisp-demos)
    (add-to-list 'e:dotspacemacs-additional-packages 'evil-easymotion)
    (add-to-list 'e:dotspacemacs-additional-packages 'evil-owl)
    (add-to-list 'e:dotspacemacs-additional-packages 'helm-tramp)
    (add-to-list 'e:dotspacemacs-additional-packages 'helpful)
    (add-to-list 'e:dotspacemacs-additional-packages 'leaf)
    (add-to-list 'e:dotspacemacs-additional-packages 'leaf-tree)
    (add-to-list 'e:dotspacemacs-additional-packages 'persistent-scratch)
    (add-to-list 'e:dotspacemacs-additional-packages 'poly-org)
    (add-to-list 'e:dotspacemacs-additional-packages 'visual-regexp)
    (add-to-list 'e:dotspacemacs-additional-packages 'vlf))
  (progn ;; その他、先に設定しておきたいもの
    (set-variable 'custom-file (expand-file-name "custom.el" e:private-directory))
    (set-variable 'spacemacs-env-vars-file (expand-file-name "spacemacs.env" e:private-directory))
    (set-variable 'viper-mode nil)
    (set-variable 'which-key-enable-extended-define-key t)
    (set-variable 'which-key-show-early-on-C-h t)))
