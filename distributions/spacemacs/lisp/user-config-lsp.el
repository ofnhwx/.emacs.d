;;; user-config-lsp.el

(leaf lsp-mode
  :defer-config
  (e:cache! lsp-intelephense-storage-path "lsp/cache")
  (e:cache! lsp-server-install-dir "lsp/server")
  (e:cache! lsp-session-file "lsp/session.v1")
  (e:variable! lsp-enable-file-watchers nil)
  (e:variable! lsp-file-watch-threshold 100000)
  (e:variable! lsp-headerline-breadcrumb-enable nil)
  (e:variable! lsp-modeline-code-actions-enable nil))

(leaf lsp-ui-doc
  :defer-config
  (e:variable! lsp-ui-doc-delay 2.0)
  (e:variable! lsp-ui-doc-position 'at-point)
  (e:variable! lsp-ui-doc-show-with-cursor t))

(leaf lsp-completion
  :after lsp-mode
  :hook (lsp-completion-mode-hook . e:setup-lsp-completion-config)
  :config
  (defun e:setup-lsp-completion-config ()
    (let ((backends (cl-case major-mode
                      ((enh-ruby-mode ruby-mode)
                       '(company-capf company-robe :with company-tabnine)))))
      (if backends
          (setq-local company-backends (-concat (list backends) spacemacs-default-company-backends))))))

(leaf lsp-diagnostics
  :after lsp-mode
  :hook (lsp-diagnostics-mode-hook . e:setup-lsp-diagnostics-config)
  :config
  (defun e:setup-lsp-diagnostics-config ()
    (let ((checker (cl-case major-mode
                     ((enh-ruby-mode ruby-mode)
                      (unless (e:bundle-exists "solargraph")
                        'ruby-rubocop)))))
      (and checker
           (flycheck-may-enable-checker checker)
           (flycheck-select-checker checker)))))

(leaf dap-mode
  :defer-config
  (e:cache! dap-breakpoints-file "dap/breakpoints")
  (e:cache! dap-utils-extension-path "dap/extensions"))



(leaf lsp-solargraph
  :defer-config
  (define-advice lsp-solargraph--build-command (:before () auto-detect)
    (setq-local lsp-solargraph-use-bundler (e:bundle-exists "solargraph")))
  (e:variable! lsp-solargraph-library-directories '("~/.asdf/installs/ruby")))

(leaf lsp-graphql
  :defer-config
  (setf (lsp--client-activation-fn (ht-get lsp-clients 'graphql-lsp)) nil))

(leaf web-mode
  :defer-config
  (require 'lsp-volar))



(provide 'user-config-lsp)

;;; user-config-lsp.el ends here
