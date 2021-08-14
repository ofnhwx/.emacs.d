;;; user-config-js.el

(leaf web-mode ;; typescript-tsx-mode
  :hook (typescript-tsx-mode-hook . setup-typescript-tsx-mode)
  :config
  (defun setup-typescript-tsx-mode ()
    (set-variable 'web-mode-code-indent-offset 2)
    (set-variable 'web-mode-css-indent-offset 2)
    (set-variable 'web-mode-markup-indent-offset 2)
    (set-variable 'web-mode-sql-indent-offset 2)))

(provide 'user-config-js)

;;; user-config-js.el ends here
