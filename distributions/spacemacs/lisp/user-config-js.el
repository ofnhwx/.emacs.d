;;; user-config-js.el

(leaf web-mode ;; typescript-tsx-mode, vue-mode
  :hook ((typescript-tsx-mode-hook . setup-typescript-tsx-mode)
         (vue-mode-hook . setup-vue-mode))
  :config
  (defun setup-typescript-tsx-mode ()
    (e:variable! web-mode-code-indent-offset   2)
    (e:variable! web-mode-css-indent-offset    2)
    (e:variable! web-mode-markup-indent-offset 2)
    (e:variable! web-mode-sql-indent-offset    2)
    (poly-tsx-mode))
  (defun setup-vue-mode ()
    (poly-vue-mode)))

(leaf graphql-mode
  :hook (graphql-mode-hook . setup-graphql-mode)
  :config
  (defun setup-graphql-mode ()
    (lsp-deferred)))



(leaf polymode
  :commands (poly-tsx-mode poly-vue-mode)
  :config
  (define-innermode poly-graphql-innermode
    :mode 'graphql-mode
    :head-matcher "gql`"
    :tail-matcher "`"
    :head-mode 'host
    :tail-mode 'host)
  
  (define-hostmode poly-tsx-hostmode
    :mode 'typescript-tsx-mode)
  (define-polymode poly-tsx-mode
    :hostmode 'poly-tsx-hostmode
    :innermodes '(poly-graphql-innermode))
  
  (define-hostmode poly-vue-hostmode
    :mode 'vue-mode)
  (define-polymode poly-vue-mode
    :hostmode 'poly-vue-hostmode
    :innermodes '(poly-graphql-innermode)))



(leaf lsp-mode
  ;; yarn global add graphql graphql-language-service-cli
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
