;;; spacemacs/bootstrap.el
;;; Commentary:
;;; Code:

(define-advice dotspacemacs/layers (:after (&rest _) custom)
  "Customize `dotspacemacs/layers'."
  (setq dotspacemacs-configuration-layers
        '(
          ;; +checkers
          spell-checking
          syntax-checking
          ;; +completion
          (auto-completion :variables
                           auto-completion-enable-help-tooltip t
                           auto-completion-use-company-box t)
          helm
          ;; +emacs
          (org :variables
               org-enable-roam-support t)
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
          (javascript :variables
                      javascript-backend 'lsp)
          (php :variables
               php-backend 'lsp)
          (ruby :variables
                ruby-backend 'lsp
                ruby-test-runner 'rspec)
          (typescript :variables
                      typescript-backend 'lsp)
          csv
          emacs-lisp
          html
          markdown
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
               git-enable-magit-gitflow-plugin t
               git-magit-status-fullscreen t)
          (version-control :variables
                           version-control-diff-tool 'diff-hl)
          github
          ;; +spacemacs
          (spacemacs-completion :variables
                                helm-use-fuzzy nil)
          ;; +tags
          gtags
          ;; +themes
          colors
          ;; +tools
          (shell :variables
                 shell-default-position 'full
                 shell-default-height 60
                 shell-default-shell 'vterm)
          dap
          docker
          lsp
          nginx
          prodigy
          restclient
          vagrant
          ;; +web
          eww
          ;; +web-services
          search-engine
          ;; +private
          misc
          ))
  (setq dotspacemacs-additional-packages
        '(
          (bitwarden            :location (recipe :fetcher github :repo "ofnhwx/emacs-bitwarden"))
          (command-logger       :location (recipe :fetcher github :repo "ofnhwx/command-logger"))
          (helm-fzf             :location (recipe :fetcher github :repo "ofnhwx/helm-fzf"))
          (komunan-lisp-library :location (recipe :fetcher github :repo "ofnhwx/komunan-lisp-library"))
          (locale-eaw           :location (recipe :fetcher github :repo "ofnhwx/locale-eaw"))
          (ls-lisp-extension    :location (recipe :fetcher github :repo "ofnhwx/ls-lisp-extension"))
          ))
  (setq dotspacemacs-excluded-packages
        '(
          ;; +spacemacs/spacemacs-editing
          dired-quick-sort
          ;; +spacemacs/spacemacs-evil
          evil-escape
          ;; +spacemacs/spacemacs-navigation
          golden-ratio
          ;; +checkers/syntax-checking
          flycheck-pos-tip
          ;; +completion/auto-completion
          auto-yasnippet
          auto-complete
          ac-ispell
          fuzzy
          yasnippet-snippets
          ;; +completion/helm
          helm-flx
          helm-themes
          ;; +intl/japanese
          pangu-spacing
          ;; +lang/php
          company-php
          php-auto-yasnippets
          phpcbf
          ;; +themes/colors
          nyan-mode
          )))

(define-advice dotspacemacs/init (:after (&rest _) custom)
  "Customize `dotspacemacs/init'."
  (setq dotspacemacs-default-font '("Cica" :size 12.0 :weight normal :width normal))
  (setq dotspacemacs-editing-style 'hybrid)
  (setq dotspacemacs-enable-server t)
  (setq dotspacemacs-scratch-buffer-persistent t)
  (setq dotspacemacs-scratch-mode 'lisp-interaction-mode)
  (setq dotspacemacs-scroll-bar-while-scrolling nil))

(define-advice dotspacemacs/user-init (:after (&rest _) custom)
  "Customize `dotspacemacs/user-init'."
  (prog1 "これは後だと間に合わないのでここで設定"
    ;; +distributions/spacemacs-bootstrap
    (set-variable 'vim-style-remap-Y-to-y$ t)
    ;; +emacs/org
    (set-variable 'org-roam-directory (expand-file-name "org/roam" e:private-directory))
    (set-variable 'org-roam-db-location (expand-file-name "org-roam.db" e:private-directory)))
  (prog1 "その他、先に設定しておきたいもの"
    (set-variable 'custom-file null-device)
    (set-variable 'evil-want-keybinding nil)
    (set-variable 'frame-resize-pixelwise t)
    (set-variable 'package-gnupghome-dir (expand-file-name "gnupg" spacemacs-cache-directory))
    (set-variable 'spacemacs-env-vars-file (expand-file-name "spacemacs.env" spacemacs-cache-directory))
    (set-variable 'viper-mode nil)
    (set-variable 'which-key-enable-extended-define-key t)
    (set-variable 'which-key-show-early-on-C-h t)
    (set-variable 'window-resize-pixelwise t)
    (setq-default ispell-local-dictionary "en_US"))
  (prog1 "skk でカーソルの色が変わらなくなったので暫定対応"
    (require 'facemenu)))

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
