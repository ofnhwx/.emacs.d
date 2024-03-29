;;; packages.el

(defvar custom-completions-packages
  '(
    (fzf-native :location (recipe :fetcher github :repo "dangduc/fzf-native" :files (:defaults "bin")))
    (copilot    :location (recipe :fetcher github :repo "zerolfx/copilot.el" :files (:defaults "dist")))
    cape
    company-org-block
    company-tabnine
    corfu
    corfu-prescient
    fussy
    kind-icon
    orderless
    ))

(defun custom-completions/init-cape ()
  (use-package cape
    :defer (spacemacs/defer)))

(defun custom-completions/init-company-org-block ()
  (use-package company-org-block
    :after (company org)
    :config
    (set-variable 'company-org-block-edit-style 'inline)
    (with-no-warnings (spacemacs|add-company-backends :backends company-org-block :modes org-mode))))

(defun custom-completions/init-company-tabnine ()
  (use-package company-tabnine
    :defer (spacemacs/defer)))

(defun custom-completions/init-copilot ()
  (use-package copilot
    :diminish (copilot-mode  "")
    :hook (prog-mode . copilot-mode)
    :bind (:map copilot-mode-map
                ("C-z" . copilot-complete)
                ("<backtab>" . copilot-complete)
                :map copilot-completion-map
                ("<escape>" . copilot-clear-overlay)
                ("C-n" . copilot-next-completion)
                ("C-p" . copilot-previous-completion))
    :config
    ;; (add-to-list 'copilot-enable-predicates 'ignore)
    (defun copilot-accept-completion-func (&rest _)
      (copilot-accept-completion))
    (with-eval-after-load 'corfu
      (advice-add 'corfu-complete :before-until 'copilot-accept-completion-func))
    (advice-add 'indent-for-tab-command :before-until 'copilot-accept-completion-func)))

(defun custom-completions/init-corfu ()
  (use-package corfu
    :defer (spacemacs/defer)
    :bind (:map corfu-map
                ("<escape>" . corfu-quit)
                ("C-q" . corfu-quick-complete))
    :hook ((prog-mode . e:setup-capf/default)
           (org-mode  . e:setup-capf/org)
           (lsp-completion-mode . e:setup-capf/lsp)
           (corfu-mode . corfu-echo-mode)
           (corfu-mode . corfu-popupinfo-mode))
    :init
    (set-variable 'corfu-auto t)
    (set-variable 'corfu-auto-prefix 1)
    (spacemacs/defer-until-after-user-config #'global-corfu-mode)
    :config
    (e:disable-company t)
    (with-eval-after-load 'evil
      (evil-make-overriding-map corfu-map)
      (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
      (advice-add 'corfu--teardown :after 'evil-normalize-keymaps))))

(defun custom-completions/init-corfu-prescient ()
  (use-package corfu-prescient
    :defer (spacemacs/defer)
    :hook ((corfu-prescient-mode . prescient-persist-mode))
    :init
    (set-variable 'prescient-save-file (expand-file-name "prescient-save.el" spacemacs-cache-directory))
    (set-variable 'prescient-aggressive-file-save t)
    (set-variable 'corfu-prescient-completion-styles '(fussy))
    (spacemacs/defer-until-after-user-config #'corfu-prescient-mode)))

(defun custom-completions/init-fussy ()
  (use-package fussy
    :init
    (setq completion-styles '(fussy))
    (setq completion-category-defaults nil)
    (setq completion-category-overrides nil)
    :config
    (set-variable 'fussy-filter-fn 'fussy-filter-orderless)
    (set-variable 'fussy-score-fn 'fussy-fzf-native-score)
    (set-variable 'fussy-max-candidate-limit 5000)))

(defun custom-completions/init-fzf-native ()
  (use-package fzf-native
    :config
    (fzf-native-load-dyn)))

(defun custom-completions/init-kind-icon ()
  (use-package kind-icon
    :after (corfu)
    :config
    (set-variable 'kind-icon-default-face 'corfu-default)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(defun custom-completions/init-orderless ()
  (use-package orderless
    :defer (spacemacs/defer)))
