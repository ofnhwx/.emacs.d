;;; packages.el

(defvar completions-packages
  '(
    (fzf-native :location (recipe :fetcher github :repo "dangduc/fzf-native" :files (:defaults "bin")))
    (copilot    :location (recipe :fetcher github :repo "zerolfx/copilot.el" :files (:defaults "dist")))
    cape
    company-org-block
    company-prescient
    company-tabnine
    corfu
    corfu-doc
    fussy
    kind-icon
    orderless
    ))

(defun completions/init-cape ()
  (use-package cape
    :defer (spacemacs/defer)))

(defun completions/init-company-org-block ()
  (use-package company-org-block
    :after (company org)
    :config
    (set-variable 'company-org-block-edit-style 'inline)
    (with-no-warnings (spacemacs|add-company-backends :backends company-org-block :modes org-mode))))

(defun completions/init-company-prescient ()
  (use-package company-prescient
    :after (company)
    :config
    (company-prescient-mode 1)))

(defun completions/init-company-tabnine ()
  (use-package company-tabnine
    :defer (spacemacs/defer)
    :config
    (set-variable 'company-tabnine-binaries-folder
                  (expand-file-name "tabnine" spacemacs-cache-directory))))

(defun completions/init-copilot ()
  (use-package copilot
    :diminish (copilot-mode  "")
    :hook (prog-mode . copilot-mode)
    :bind (:map copilot-mode-map
                ("C-z" . copilot-complete)
                :map copilot-completion-map
                ("<escape>" . copilot-clear-overlay)
                ("C-n" . copilot-next-completion)
                ("C-p" . copilot-previous-completion))
    :config
    (add-to-list 'copilot-enable-predicates 'ignore)
    (defalias 'copilot-accept-completion-func #'copilot-accept-completion-by-word)
    (with-eval-after-load 'corfu
      (advice-add 'corfu-complete :before-until 'copilot-accept-completion-func))
    (advice-add 'indent-for-tab-command :before-until 'copilot-accept-completion-func)))

(defun completions/init-corfu ()
  (use-package corfu
    :defer (spacemacs/defer)
    :bind (:map corfu-map
                ("<escape>" . corfu-quit))
    :hook ((prog-mode . e:setup-capf/default)
           (org-mode  . e:setup-capf/org)
           (lsp-completion-mode . e:setup-capf/lsp))
    :init
    (set-variable 'corfu-auto t)
    (spacemacs/defer-until-after-user-config #'global-corfu-mode)
    :config
    (e:disable-company t)
    (with-eval-after-load 'evil
      (evil-make-overriding-map corfu-map)
      (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
      (advice-add 'corfu--teardown :after 'evil-normalize-keymaps))))

(defun completions/init-corfu-doc ()
  (use-package corfu-doc
    :hook (corfu-mode . corfu-doc-mode)))

(defun completions/init-fussy ()
  (use-package fussy
    :init
    (setq completion-styles '(fussy))
    (setq completion-category-defaults nil)
    (setq completion-category-overrides nil)
    :config
    (set-variable 'fussy-filter-fn 'fussy-filter-orderless)
    (set-variable 'fussy-score-fn 'fussy-fzf-native-score)
    (set-variable 'fussy-max-candidate-limit 5000)))

(defun completions/init-fzf-native ()
  (use-package fzf-native
    :config
    (fzf-native-load-dyn)))

(defun completions/init-kind-icon ()
  (use-package kind-icon
    :after (corfu)
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(defun completions/init-orderless ()
  (use-package orderless
    :defer (spacemacs/defer)))
