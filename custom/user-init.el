
;; spacemacs の設定
(progn
  ;; +distributions/spacemacs-bootstrap
  (set-variable 'vim-style-remap-Y-to-y$ t)
  ;; +spacemacs/spacemacs-completion
  (set-variable 'helm-use-fuzzy nil)
  )

;; configuration-layers
(defvar e:dotspacemacs-configuration-layers nil)
(setq e:dotspacemacs-configuration-layers
      `(
        ;; +checkers
        syntax-checking
        ;; +completion
        (auto-completion :variables
                         auto-completion-enable-help-tooltip nil
                         auto-completion-enable-sort-by-usage t
                         auto-completion-use-company-box t)
        helm
        ;; +emacs
        better-defaults
        org
        ;; +filetree
        treemacs
        ;; +frameworks
        react
        ruby-on-rails
        ;; +fun
        emoji
        ;; +intl
        japanese
        ;; +lang
        csv
        emacs-lisp
        html
        javascript
        markdown
        php
        (ruby :variables
              ruby-backend 'lsp)
        shell-scripts
        sql
        vimscript
        yaml
        ;; +misc
        copy-as-format
        multiple-cursors
        ;; +source-control
        (git :variables
             git-magit-status-fullscreen t)
        github
        ;; +tags
        gtags
        ;; +tools
        docker
        lsp
        nginx
        prodigy
        restclient
        (shell :variables
               shell-enable-smart-eshell t
               shell-default-height 60
               shell-default-shell 'vterm)
        vagrant
        ;; +web-services
        search-engine
        ))

;; additional-packages
(defvar e:dotspacemacs-additional-packages nil)
(setq e:dotspacemacs-additional-packages
      '(
        (helm-fzf     :location (recipe :fetcher github :repo "ofnhwx/helm-fzf"))
        (locale-eaw   :location (recipe :fetcher github :repo "hamano/locale-eaw"))
        atomic-chrome
        codic
        company-tabnine
        company-try-hard
        dired-filter
        elisp-demos
        evil-easymotion
        evil-owl
        helm-tramp
        helpful
        leaf
        leaf-tree
        persistent-scratch
        poly-org
        visual-regexp
        vlf
        ))

;; excluded-packages
(defvar e:dotspacemacs-excluded-packages nil)
(setq e:dotspacemacs-excluded-packages
      '(
        ;; +spacemacs/spacemacs-evil
        evil-escape
        ;; +intl/japanese
        pangu-spacing
        ;; +lang/php
        company-php
        ))

;; その他、先に設定しておきたいもの
(progn
  (set-variable 'custom-file (expand-file-name "custom.el" e:private-directory))
  (set-variable 'spacemacs-env-vars-file (expand-file-name "spacemacs.env" e:private-directory))
  (set-variable 'viper-mode nil)
  (set-variable 'which-key-enable-extended-define-key t)
  (set-variable 'which-key-show-early-on-C-h t)
  )
