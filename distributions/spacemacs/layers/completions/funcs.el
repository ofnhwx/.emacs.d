;;; funcs.el

(defun e:capf-functions (default-capf)
  (let ((default-capf-with-tabnine (intern (format "%s-with-tabnine" default-capf))))
    (defalias 'e:cape-tabnine
      (cape-company-to-capf #'company-tabnine))
    (defalias default-capf-with-tabnine
      (cape-super-capf
       default-capf
       #'e:cape-tabnine))
    (list #'cape-file
          default-capf-with-tabnine
          #'cape-dabbrev)))

(defun e:setup-capf/default ()
  (setq-local completion-at-point-functions
              (e:capf-functions (car completion-at-point-functions))))

(defun e:setup-capf/org ()
  (defalias 'e:cape-org-block
    (cape-company-to-capf #'company-org-block))
  (setq-local completion-at-point-functions
              (e:capf-functions #'e:cape-org-block)))

(defun e:setup-capf/lsp ()
  (setq-local completion-at-point-functions
              (e:capf-functions #'lsp-completion-at-point)))

(defun e:disable-company (disabled)
  (if disabled
      (advice-add 'company-mode :override 'ignore)
    (advice-remove 'company-mode 'ignore)))
