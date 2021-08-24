;;; user-config-js.el

(leaf web-mode ;; typescript-tsx-mode, vue-mode
  :hook ((typescript-tsx-mode-hook . setup-typescript-tsx-mode)
         (vue-mdoe . setup-vue-mode))
  :config
  (defun setup-typescript-tsx-mode ()
    (e:variable! web-mode-code-indent-offset   2)
    (e:variable! web-mode-css-indent-offset    2)
    (e:variable! web-mode-markup-indent-offset 2)
    (e:variable! web-mode-sql-indent-offset    2))
  (defun setup-vue-mode ()
    (when (fboundp 'lsp-flycheck-add-mode)
      (lsp-flycheck-add-mode 'vue-mode))))

(leaf graphql-mode
  :hook (graphql-mode-hook . setup-graphql-mode)
  :config
  (defun setup-graphql-mode ()
    (lsp-deferred)))



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
