;;; user-config-js.el

(leaf web-mode ;; typescript-tsx-mode, vue-mode
  :hook ((typescript-tsx-mode-hook . setup-web-mode)
         (vue-mode-hook . setup-vue-mode))
  :config
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp--formatting-indent-alist '(typescript-tsx-mode . typescript-indent-level)))
  (defun setup-web-mode ()
    (e:default! typescript-indent-level       2)
    (e:default! web-mode-markup-indent-offset 2)
    (e:default! web-mode-css-indent-offset    2)
    (e:default! web-mode-code-indent-offset   2)
    (e:default! web-mode-attr-indent-offset   2)
    (e:default! web-mode-enable-auto-indentation nil))
  (defun setup-vue-mode ()
    (with-eval-after-load 'lsp-mode
      (lsp-flycheck-add-mode 'vue-mode)))
  (setup-web-mode))

(leaf graphql-mode
  :hook (graphql-mode-hook . setup-graphql-mode)
  :config
  (defun setup-graphql-mode ()
    (lsp-deferred)))

(provide 'user-config-js)

;;; user-config-js.el ends here
