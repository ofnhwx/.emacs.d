;;; user-init.el

(defvar custom-configuration-layers nil)
(setq custom-configuration-layers
      '(
        ;; +checkers
        spell-checking
        syntax-checking
        ;; +completion
        (auto-completion
         :variables
         auto-completion-enable-help-tooltip t
         auto-completion-enable-sort-by-usage t
         auto-completion-use-company-box t)
        helm
        ;; +emacs
        (org
         :variables
         org-enable-roam-support t)
        better-defaults
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
        (javascript
         :variables
         javascript-backend 'lsp)
        (php
         :variables
         php-backend 'lsp)
        (ruby
         :variables
         ruby-backend 'lsp
         ruby-enable-enh-ruby-mode nil
         ruby-test-runner 'rspec
         ruby-highlight-debugger-keywords t)
        csv
        emacs-lisp
        html
        markdown
        python
        shell-scripts
        sql
        typescript
        vimscript
        yaml
        ;; +misc
        copy-as-format
        dtrt-indent
        multiple-cursors
        ;; +source-control
        (git
         :variables
         git-magit-status-fullscreen t
         git-enable-magit-delta-plugin t)
        (version-control
         :variables
         version-control-diff-tool 'diff-hl)
        github
        ;; +spacemacs
        (spacemacs-completion
         :variables
         helm-use-fuzzy nil)
        ;; +tags
        gtags
        ;; +tools
        (shell
         :variables
         shell-default-position 'full
         shell-default-height 60
         shell-default-shell 'vterm)
        docker
        lsp
        nginx
        prodigy
        restclient
        vagrant
        ;; +web-services
        search-engine
        ;; +private
        misc
        ))

(defvar custom-additional-packages nil)
(setq custom-additional-packages
      '(
        (bitwarden            :location (recipe :fetcher github :repo "ofnhwx/emacs-bitwarden"))
        (command-logger       :location (recipe :fetcher github :repo "ofnhwx/command-logger"))
        (helm-fzf             :location (recipe :fetcher github :repo "ofnhwx/helm-fzf"))
        (komunan-lisp-library :location (recipe :fetcher github :repo "ofnhwx/komunan-lisp-library"))
        (ls-lisp-extension    :location (recipe :fetcher github :repo "ofnhwx/ls-lisp-extension"))
        (locale-eaw           :location (recipe :fetcher github :repo "hamano/locale-eaw"))
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

(defun user-init-config ()
  (prog1 "これは後だと間に合わないのでここで設定"
    ;; +distributions/spacemacs-bootstrap
    (set-variable 'vim-style-remap-Y-to-y$ t))
  (prog1 "その他、先に設定しておきたいもの"
    (set-variable 'custom-file null-device)
    (set-variable 'spacemacs-env-vars-file (expand-file-name "spacemacs.env" spacemacs-cache-directory))
    (set-variable 'viper-mode nil)
    (set-variable 'which-key-enable-extended-define-key t)
    (set-variable 'which-key-show-early-on-C-h t)
    (setq-default ispell-local-dictionary "en_US"))
  (prog1 "yasnippet で余計なものを読込ませないための小細工"
    (defvar e:yas-snippet-dirs (list (expand-file-name "snippets" e:custom-directory)))
    (dolist (dir e:yas-snippet-dirs)
      (unless (file-exists-p dir)
        (make-directory dir)))
    (with-eval-after-load "yasnippet"
      (define-advice yas-reload-all (:around (fn &rest args) only-custom-snippets)
        (when (equal yas-snippet-dirs e:yas-snippet-dirs)
          (funcall fn args))))))

(provide 'user-init.el)

;;; user-init.el ends here
