;;; user-config-php.el

(leaf php-mode
  :hook (php-mode-hook . e:setup-php-mode)
  :config
  (defun e:setup-php-mode ()
    (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
    (subword-mode 1)))

(leaf php-mode
  :after dap-mode
  :defun (dap-register-debug-template)
  :config
  (dap-register-debug-template
   "Remote Xdebug"
   (list :name "Remote Xdebug"
         :type "php"
         :request "launch"
         :port 9000
         :pathMappings (list :/var/www/html "${workspaceFolder}"))))

(leaf drupal-mode
  :after php-mode
  :hook (drupal-mode-hook . e:setup-drupal-mode)
  :config
  (spacemacs|diminish drupal-mode "")
  (defun e:setup-drupal-mode ()
    (let* ((project-root (ignore-errors (kllib:project-root buffer-file-name)))
           (eslint (f-expand "web/core/node_modules/.bin/eslint" project-root)))
      (when (and project-root
                 (f-exists? project-root)
                 (f-exists? eslint))
        (setq-local flycheck-javascript-eslint-executable eslint)))))

(leaf drupal/phpcs
  :after php-mode
  :defer-config
  (set-variable 'drupal/phpcs-standard "Drupal,DrupalPractice"))

(provide 'user-config-php)

;;; user-config-php.el ends here
