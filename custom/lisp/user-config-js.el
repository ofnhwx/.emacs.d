;;; user-config-js.el

(leaf js2-mode
  :defer-config
  (set-face-attribute 'js2-external-variable nil :foreground "#ff0000" :underline t)
  (defun e:eslint-fix ()
    (interactive)
    (let ((eslint (or flycheck-javascript-eslint-executable
                      (executable-find "eslint"))))
      (when eslint
        (call-process eslint nil nil nil buffer-file-name "--fix")))))

(provide 'user-config-js)

;;; user-config-js.el ends here
