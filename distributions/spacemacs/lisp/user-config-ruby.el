;;; user-config-ruby.el

(leaf ruby-refactor
  :defer-config
  (spacemacs|diminish ruby-refactor-mode))

(leaf projectile-rails
  :init
  (let ((exts '("html" "erb" "haml" "slim"
                "js" "coffee" "ts"
                "css" "scss" "sass" "less"
                "json" "builder" "jbuilder" "rabl"
                "csb" "axlsx")))
    (e:variable! projectile-rails-views-re (concat "\\." (regexp-opt exts))))
  :defer-config
  (defun projectile-rails-find-view-components ()
    "Find a View component."
    (interactive)
    (projectile-rails-find-resource
     "components: "
     `(("app/components/" "\\(.+\\)\\.rb$")
       ("app/components/" ,(concat "\\(.+\\)" projectile-rails-views-re)))
     "app/components/${filename}"))
  (defun projectile-rails-find-graphql ()
    "Find a GraphQL."
    (interactive)
    (projectile-rails-find-resource
     "graphql: "
     '(("app/graphql/" "\\(.+\\)\\.rb$"))
     "app/graphql/${filename}"))
  (define-advice projectile-rails-find-component (:override ())
    (interactive)
    (projectile-rails-find-resource
     "component: "
     (->> '("app/frontend/" "app/javascript")
          (--filter (f-exists? (f-expand it (projectile-rails-root))))
          (--map (list it "\\(.+\\.[^.]+\\)$")))))
  (spacemacs/set-leader-keys-for-minor-mode 'projectile-rails-mode
    "fff" 'projectile-rails-find-component
    "ffg" 'projectile-rails-find-graphql
    "ffC" 'projectile-rails-find-view-components
    "ffF" 'projectile-rails-find-feature
    "ffS" 'projectile-rails-find-serializer
    "ffV" 'projectile-rails-find-validator
    ))

(leaf ruby-mode
  :hook (ruby-mode-hook . e:setup-flycheck-rubocop)
  :mode `(,(concat "\\." (regexp-opt '("simplecov" "jb" "csb" "axlsx")) "\\'"))
  :config
  (e:variable! ruby-insert-encoding-magic-comment nil)
  
  (progn
    (defvar e:bundle-exists-cache (make-hash-table :test 'equal))
    (defun e:bundle-exists (name)
      (let ((key (format "%s@%s" name (or (kllib:project-root) (buffer-name)))))
        (unless (gethash key e:bundle-exists-cache)
          (puthash key (call-process-shell-command (format "bundle info %s" name)) e:bundle-exists-cache))
        (zerop (gethash key e:bundle-exists-cache)))))
  (defun e:setup-flycheck-rubocop ()
    (when (e:bundle-exists "rubocop")
      (setq-local flycheck-command-wrapper-function
                  (lambda (command)
                    (if (s-ends-with? "rubocop" (car command))
                        (append '("bundle" "exec") command)
                      command))))))

(leaf ruby-mode
  :after grugru
  :defer-config
  (grugru-define-multiple
   (ruby-mode
    (symbol "have_button" "have_no_button")
    (symbol "have_content" "have_no_content")
    (symbol "have_link" "have_no_link")
    (symbol "if" "unless")
    (symbol "let" "let!")
    (symbol "to" "not_to")
    (symbol "true" "false"))))

(leaf ruby-tools
  :bind (:ruby-tools-mode-map
         ("C-:" . nil)
         ("C-;" . nil)))

(leaf robe
  :hook (ruby-mode-hook . robe-mode)
  :commands (robe-start robe-ask robe-doc robe-jump robe-jump-to-module robe-rails-refresh)
  :config
  (spacemacs|diminish robe-mode)
  (spacemacs/declare-prefix-for-mode 'ruby-mode "mr" "refactor/robe")
  (spacemacs/declare-prefix-for-mode 'ruby-mode "mrs" "robe")
  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
    "rss" #'robe-start
    "rsa" #'robe-ask
    "rsd" #'robe-doc
    "rsj" #'robe-jump
    "rsm" #'robe-jump-to-module
    "rsr" #'robe-rails-refresh))

(leaf rubocop
  :defer-config
  (spacemacs|diminish rubocop-mode)
  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
    "RF" 'rubocop-autocorrect-current-file))

(leaf rubocopfmt
  :config
  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
    "==" 'rubocopfmt)
  :defer-config
  (e:variable! rubocopfmt-use-bundler-when-possible t))

(leaf haml-mode
  :init
  (spacemacs|add-company-backends
    :backends (company-capf company-tabnine)
    :modes haml-mode))

(leaf haml-mode
  :after flycheck
  :defer-config
  (flycheck-def-config-file-var flycheck-haml-lintrc haml-lint ".haml-lint.yml" :safe #'stringp)
  (flycheck-define-checker haml-lint
    "A haml-lint syntax checker"
    :command ("bundle" "exec" "haml-lint"
              (config-file "--config" flycheck-haml-lintrc)
              source-inplace)
    :error-patterns
    ((error   line-start (file-name) ":" line " [E] " (message) line-end)
     (warning line-start (file-name) ":" line " [W] " (message) line-end))
    :modes (haml-mode))
  (add-to-list 'flycheck-checkers 'haml-lint)
  (flycheck-add-next-checker 'haml 'haml-lint))

(provide 'user-config-ruby)

;;; user-config-ruby.el ends here
