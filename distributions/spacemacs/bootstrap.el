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
          (auto-completion :variables auto-completion-enable-help-tooltip t auto-completion-use-company-box t)
          helm
          ;; +emacs
          (org :variables org-enable-roam-support t)
          ;; +filetree
          treemacs
          ;; +frameworks
          react
          ruby-on-rails
          (vue :variables vue-backend 'lsp)
          ;; +fun
          emoji
          ;; +intl
          japanese
          ;; +lang
          (javascript :variables javascript-backend 'lsp)
          (php        :variables php-backend        'lsp)
          (ruby       :variables ruby-backend       'lsp ruby-test-runner 'rspec)
          (typescript :variables typescript-backend 'lsp)
          csv
          emacs-lisp
          graphql
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
          (git :variables git-enable-magit-gitflow-plugin t git-magit-status-fullscreen t)
          (version-control :variables version-control-diff-tool 'diff-hl)
          github
          ;; +spacemacs
          (spacemacs-completion :variables helm-use-fuzzy nil)
          ;; +tags
          gtags
          ;; +themes
          colors
          ;; +tools
          (shell :variables shell-default-position 'full shell-default-height 60 shell-default-shell 'vterm)
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
          (command-logger       :location (recipe :fetcher github :repo "ofnhwx/command-logger"))
          (komunan-lisp-library :location (recipe :fetcher github :repo "ofnhwx/komunan-lisp-library"))
          (locale-eaw           :location (recipe :fetcher github :repo "ofnhwx/locale-eaw"))
          (ls-lisp-extension    :location (recipe :fetcher github :repo "ofnhwx/ls-lisp-extension"))
          (key-chord            :location (recipe :fetcher github :repo "conao3/key-chord"))
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
  (cond
   ((eq system-type 'darwin)
    (setq dotspacemacs-default-font '("Cica" :size 16.0)))
   (t
    (setq dotspacemacs-default-font '("Cica" :size 12.0))))
  (setq dotspacemacs-editing-style 'hybrid)
  (setq dotspacemacs-enable-server t)
  (setq dotspacemacs-loading-progress-bar nil)
  (setq dotspacemacs-new-empty-buffer-major-mode 'fundamental-mode)
  (setq dotspacemacs-scratch-buffer-persistent t)
  (setq dotspacemacs-scratch-mode 'lisp-interaction-mode)
  (setq dotspacemacs-scroll-bar-while-scrolling nil)
  (setq dotspacemacs-startup-lists nil)
  (setq dotspacemacs-use-SPC-as-y t))

(define-advice dotspacemacs/user-init (:after (&rest _) custom)
  "Customize `dotspacemacs/user-init'."
  (prog1 "spacemacs より先に設定しておきたいもの"
    ;; +distributions/spacemacs-bootstrap
    (set-variable 'vim-style-remap-Y-to-y$ t)
    (set-variable 'which-key-enable-extended-define-key t)
    (set-variable 'which-key-show-early-on-C-h t)
    ;; +checkers/spell-checking
    (setq-default ispell-local-dictionary "en_US")
    ;; +emacs/org
    (set-variable 'org-roam-directory (expand-file-name "~/org/roam"))
    (set-variable 'org-roam-db-location (expand-file-name "org-roam.db" spacemacs-cache-directory))
    (set-variable 'org-roam-v2-ack t)
    ;; others
    (set-variable 'custom-file null-device)
    (set-variable 'frame-resize-pixelwise t)
    (set-variable 'package-gnupghome-dir (expand-file-name "gnupg" spacemacs-cache-directory))
    (set-variable 'spacemacs-env-vars-file (expand-file-name "spacemacs.env" spacemacs-cache-directory))
    (set-variable 'viper-mode nil)
    (set-variable 'window-resize-pixelwise t))
  (prog1 "暫定対応として `require' をちょっと追加"
    (require 'ert)
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
