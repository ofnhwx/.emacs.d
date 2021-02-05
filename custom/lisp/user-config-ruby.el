;;; user-config-ruby.el

(defun e:bundle-exists (name)
  (and (executable-find "bundle")
       (zerop (call-process-shell-command (format "bundle info %s" name)))))

(leaf ruby-mode
  :hook (ruby-mode-hook . e:setup-flycheck-rubocop)
  :defer-config
  (set-variable 'ruby-insert-encoding-magic-comment nil)
  (defun e:setup-flycheck-rubocop ()
    (when (e:bundle-exists "rubocop")
      (setq-local flycheck-command-wrapper-function
                  (lambda (command)
                    (append '("bundle" "exec") command))))))

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

(leaf ruby-refactor
  :defer-config
  (spacemacs|diminish ruby-refactor-mode))

(leaf ruby-tools
  :bind (:ruby-tools-mode-map
         ("C-;" . nil)))

(leaf robe
  :hook ruby-mode-hook
  :commands (robe-start robe-ask robe-doc robe-jump robe-jump-to-module robe-rails-refresh)
  :config
  (spacemacs|diminish robe-mode)
  (spacemacs/declare-prefix-for-mode ruby-mode-map "mr" "refactor/robe")
  (spacemacs/declare-prefix-for-mode ruby-mode-map "mrs" "robe")
  (spacemacs/set-leader-keys-for-major-mode ruby-mode-map
    "rs'" #'robe-start
    "rsa" #'robe-ask
    "rsd" #'robe-doc
    "rsj" #'robe-jump
    "rsm" #'robe-jump-to-module
    "rsr" #'robe-rails-refresh))

(leaf rubocop
  :defer-config
  (spacemacs|diminish rubocop-mode)
  (spacemacs/set-leader-keys-for-major-mode ruby-mode-map
    "RF" 'rubocop-autocorrect-current-file))

(leaf rubocopfmt
  :config
  (spacemacs/set-leader-keys-for-major-mode ruby-mode-map
    "==" 'rubocopfmt)
  :defer-config
  (set-variable 'rubocopfmt-use-bundler-when-possible t))

(leaf haml-mode
  :hook (haml-mode-hook . e:setup-haml-mode)
  :config
  (defun e:setup-haml-mode ()
    (e:setup-company-backends 'company-tabnine)
    (company-mode-on)))

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
