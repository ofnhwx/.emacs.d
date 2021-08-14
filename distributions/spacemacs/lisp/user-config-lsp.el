;;; user-config-lsp.el

(e:after! lsp-mode
  (e:cache! lsp-server-install-dir "lsp/server")
  (e:cache! lsp-session-file "lsp/session.v1")
  (e:cache! lsp-intelephense-storage-path "lsp/cache")
  (e:variable! lsp-file-watch-threshold 100000)
  (e:variable! lsp-headerline-breadcrumb-enable nil)
  (e:variable! lsp-solargraph-library-directories '("~/.asdf/installs/ruby")))

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
      ;; for PHP
      ((php-mode)
       (e:setup-company-backends '(company-capf :with company-tabnine))))))

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
      ;; for PHP
      ((php-mode)
       (when (flycheck-may-enable-checker 'php)
         (flycheck-select-checker 'php)))
      ;; for JS
      ((js2-mode)
       (when (flycheck-may-enable-checker 'javascript-eslint)
         (flycheck-select-checker 'javascript-eslint))))))

(leaf lsp-ui-doc
  :defun (lsp-ui-doc-mode)
  :defvar (lsp-ui-doc-mode e:lsp-ui-doc-mode-enabled)
  :hook ((company-completion-started-hook   . e:lsp-ui-doc-mode-temporary-disable)
         (company-completion-finished-hook  . e:lsp-ui-doc-mode-restore)
         (company-completion-cancelled-hook . e:lsp-ui-doc-mode-restore))
  :config
  (e:variable! lsp-ui-doc-position 'at-point)
  (e:variable! lsp-ui-doc-delay 2.0)
  (defun e:lsp-ui-doc-mode-temporary-disable (&rest _)
    (setq e:lsp-ui-doc-mode-enabled lsp-ui-doc-mode)
    (lsp-ui-doc-mode 0))
  (defun e:lsp-ui-doc-mode-restore (&rest _)
    (when (bound-and-true-p e:lsp-ui-doc-mode-enabled)
      (lsp-ui-doc-mode 1))))

(provide 'user-config-lsp)

;;; user-config-lsp.el ends here
