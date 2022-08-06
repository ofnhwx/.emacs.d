;;; funcs.el

(defun e:capf-functions (default-capf)
  (list #'cape-file
        (cape-capf-buster
         (cape-super-capf
          default-capf
          (cape-company-to-capf #'company-tabnine)))
        #'cape-dabbrev))

(defun e:setup-capf/default ()
  (setq-local completion-at-point-functions
              (e:capf-functions (car completion-at-point-functions))))

(defun e:setup-capf/org ()
  (setq-local completion-at-point-functions
              (e:capf-functions (cape-company-to-capf #'company-org-block))))

(defun e:setup-capf/lsp ()
  (setq-local completion-at-point-functions
              (e:capf-functions #'lsp-completion-at-point)))

(defun e:disable-company (disabled)
  (if disabled
      (advice-add 'company-mode :override 'ignore)
    (advice-remove 'company-mode 'ignore)))
