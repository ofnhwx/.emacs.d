
(defvar custom-configuration-layers nil)
(setq custom-configuration-layers
      '(
        ;; +checkers
        spell-checking
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
        ;; +email
        notmuch
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
        csv
        emacs-lisp
        html
        (javascript :variables
                    javascript-backend 'lsp)
        markdown
        (php :variables
             php-backend 'lsp)
        (python :variables
                python-backend 'lsp)
        (ruby :variables
              ruby-backend 'lsp
              ruby-enable-enh-ruby-mode nil
              ruby-test-runner 'rspec
              ruby-highlight-debugger-keywords t)
        shell-scripts
        sql
        vimscript
        yaml
        ;; +misc
        copy-as-format
        dtrt-indent
        multiple-cursors
        ;; +source-control
        (git :variables
             git-magit-status-fullscreen t
             git-enable-magit-delta-plugin t)
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
               shell-default-height 60
               shell-default-shell 'vterm)
        vagrant
        ;; +web-services
        search-engine
        ))

(defvar custom-additional-packages nil)
(setq custom-additional-packages
      '(
        (helm-fzf     :location (recipe :fetcher github :repo "ofnhwx/helm-fzf"))
        (locale-eaw   :location (recipe :fetcher github :repo "hamano/locale-eaw"))
        atomic-chrome
        codic
        company-tabnine
        company-try-hard
        deadgrep
        dired-filter
        dired-toggle-sudo
        elisp-demos
        evil-easymotion
        evil-owl
        foreman-mode
        grugru
        helm-tramp
        helpful
        leaf
        leaf-tree
        magit-libgit
        ox-reveal
        psysh
        visual-regexp
        vlf
        ))

(defvar custom-excluded-packages nil)
(setq custom-excluded-packages
      '(
        ;; +spacemacs/spacemacs-editing
        dired-quick-sort
        ;; +spacemacs/spacemacs-evil
        evil-escape
        ;; +checkers/syntax-checking
        flycheck-pos-tip
        ;; +intl/japanese
        pangu-spacing
        ;; +lang/php
        company-php
        ))

(progn
  ;; +distributions/spacemacs-bootstrap
  (set-variable 'vim-style-remap-Y-to-y$ t)
  ;; +spacemacs/spacemacs-completion
  (set-variable 'helm-use-fuzzy nil)
  ;; その他、先に設定しておきたいもの
  (set-variable 'custom-file null-device)
  (set-variable 'spacemacs-env-vars-file (expand-file-name "spacemacs.env" spacemacs-cache-directory))
  (set-variable 'viper-mode nil)
  (set-variable 'which-key-enable-extended-define-key t)
  (set-variable 'which-key-show-early-on-C-h t)
  (setq-default ispell-local-dictionary "en_US")
  ;; yasnippet で余計なものを読込ませないための対策
  (defvar e:yas-snippet-dirs (list (expand-file-name "snippets" e:custom-directory)))
  (dolist (dir e:yas-snippet-dirs)
    (unless (file-exists-p dir)
      (make-directory dir)))
  (with-eval-after-load "yasnippet"
    (define-advice yas-reload-all (:around (fn &rest args) only-custom-snippets)
      (when (equal yas-snippet-dirs e:yas-snippet-dirs)
        (funcall fn args)))))
