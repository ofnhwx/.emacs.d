;;; user-config-js.el

(leaf web-mode ;; typescript-tsx-mode, vue-mode
  :hook (typescript-tsx-mode-hook . setup-typescript-tsx-mode)
  :config
  (defun setup-typescript-tsx-mode ()
    (e:variable! web-mode-code-indent-offset   2)
    (e:variable! web-mode-css-indent-offset    2)
    (e:variable! web-mode-markup-indent-offset 2)
    (e:variable! web-mode-sql-indent-offset    2)))

(leaf graphql-mode
  :disabled t
  :hook (graphql-mode-hook . setup-graphql-mode)
  :config
  (defun setup-graphql-mode ()
    (lsp-deferred)))



(leaf mmm-mode
  :require t
  :config
  (e:variable! mmm-global-mode 'maybe)
  (mmm-add-classes
   '((embedded-graphql
      :submode graphql-mode
      :front "gql`"
      :back  "`")))
  (mmm-add-mode-ext-class 'typescript-tsx-mode nil 'embedded-graphql)
  (mmm-add-mode-ext-class 'vue-mode nil 'embedded-graphql))



(leaf lsp-mode
  ;; yarn global add graphql graphql-language-service-cli
  :defun (make-lsp-client
          lsp-register-client
          lsp-stdio-connection)
  :defvar (lsp-language-id-configuration)
  :defer-config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     (lambda ()
                                       `(,(executable-find "graphql-lsp")
                                         "server"
                                         "-m" "stream")))
                    :major-modes '(graphql-mode)
                    :server-id 'graphql))
  (add-to-list 'lsp-language-id-configuration '(graphql-mode . "graphql")))



(provide 'user-config-js)

;;; user-config-js.el ends here
