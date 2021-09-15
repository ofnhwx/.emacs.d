;;; user-config-lsp.el

(e:after! lsp-mode
  (e:cache! lsp-intelephense-storage-path "lsp/cache")
  (e:cache! lsp-server-install-dir "lsp/server")
  (e:cache! lsp-session-file "lsp/session.v1")
  (e:variable! lsp-enable-file-watchers nil)
  (e:variable! lsp-file-watch-threshold 100000)
  (e:variable! lsp-headerline-breadcrumb-enable nil)
  (e:variable! lsp-solargraph-library-directories '("~/.asdf/installs/ruby")))

(e:after! lsp-ui-doc
  (e:variable! lsp-ui-doc-position 'at-point)
  (e:variable! lsp-ui-doc-delay 2.0))

(e:after! dap-mode
  (e:cache! dap-breakpoints-file "dap/breakpoints")
  (e:cache! dap-utils-extension-path "dap/extensions"))

(leaf lsp-completion
  :after lsp-mode
  :hook (lsp-completion-mode-hook . e:setup-lsp-completion-config)
  :config
  (defun e:setup-lsp-completion-config ()
    (cl-case major-mode
      ;; for Ruby
      ((enh-ruby-mode ruby-mode)
       (e:setup-company-backends '(company-capf company-robe :with company-tabnine)))
      )))

(leaf lsp-diagnostics
  :after lsp-mode
  :hook (lsp-diagnostics-mode-hook . e:setup-lsp-diagnostics-config)
  :config
  (defun e:setup-lsp-diagnostics-config ()
    (cl-case major-mode
      ;; for Ruby
      ((enh-ruby-mode ruby-mode)
       (when (flycheck-may-enable-checker 'ruby-rubocop)
         (flycheck-select-checker 'ruby-rubocop)))
      )))



(e:after! lsp-graphql
  (setf (lsp--client-activation-fn (ht-get lsp-clients 'graphql-lsp)) nil))

(e:after! web-mode
  (require 'lsp-mode)
  (lsp-dependency 'volar-server
                  '(:system "volar-server")
                  '(:npm :package "@volar/server" :path "volar-server"))
  (defun lsp-volar--make-init-options ()
    (ht ("typescript" (ht ("serverPath" (f-expand "node_modules/typescript/lib/tsserverlibrary.js" (lsp-workspace-root)))))
        ("languageFeatures" (ht ("callHierarchy" t)
                                ("codeAction" t)
                                ("codeLens" t)
                                ("completion" (ht ("defaultTagNameCase" "both")
                                                  ("defaultAttrNameCase" "kebabCase")))
                                ("definition" t)
                                ("diagnostics" t)
                                ("documentHighlight" t)
                                ("documentLink" t)
                                ("hover" t)
                                ("references" t)
                                ("rename" t)
                                ("renameFileRefactoring" t)
                                ("schemaRequestService" t)
                                ("semanticTokens" t)
                                ("signatureHelp" t)
                                ("typeDefinition" t)))
        ("documentFeatures" (ht ("documentColor" t)
                                ("selectionRange" t)
                                ("foldingRange" t)
                                ("linkedEditingRange" t)
                                ("documentSymbol" t)
                                ("documentFormatting" (ht ("defaultPrintWidth" 100)))))))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     (lambda ()
                                       (cons (lsp-package-path 'volar-server)
                                             '("--stdio"))))
                    :major-modes '(vue-mode)
                    :initialization-options 'lsp-volar--make-init-options
                    :server-id 'volar
                    :download-server-fn (lambda (_client callback error-callback _update?)
                                          (lsp-package-ensure 'volar-server
                                                              callback error-callback)))))



(provide 'user-config-lsp)

;;; user-config-lsp.el ends here
