;;; packages.el

(defvar vertico-packages
  '(
    affe
    consult
    embark
    embark-consult
    helm
    marginalia
    orderless
    vertico
    ))

(defun vertico/init-affe ()
  (with-eval-after-load 'affe
    (when (require 'orderless nil t)
      (set-variable 'affe-regexp-function 'orderless-pattern-compiler)
      (set-variable 'affe-highlight-function 'orderless--highlight)))
  (spacemacs/set-leader-keys
    "fz" 'affe-find))

(defun vertico/init-consult ()
  (with-eval-after-load 'consult
    (set-variable 'consult-project-root-function 'kllib:project-root))
  (bind-keys
   ([remap goto-line] . consult-goto-line))
  (spacemacs|use-package-add-hook helm
    :post-init
    (-each '((helm-swoop . consult-line)
             (lazy-helm/helm-mini . consult-buffer)
             (lazy-helm/helm-recentf . consult-recent-file)
             (spacemacs/helm-M-x-fuzzy-matching . execute-extended-command)
             (spacemacs/helm-jump-in-buffer . consult-imenu))
      (lambda (item)
        (bind-key `[remap ,(car item)] (cdr item))))))

(defun vertico/init-embark ()
  (bind-key "C-c C-e" 'embark-export minibuffer-local-map))

(defun vertico/init-embark-consult ()
  (use-package embark-consult
    :after (embark consult)))

(defun vertico/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :pre-init
    (with-no-warnings (define-advice helm-mode (:around (&rest _) disable)))))

(defun vertico/init-marginalia ()
  (with-eval-after-load 'vertico
    (bind-key "M-A" 'marginalia-cycle minibuffer-local-map)
    (marginalia-mode)))

(defun vertico/init-orderless ())

(defun vertico/init-vertico ()
  (with-eval-after-load 'vertico
    (set-variable 'vertico-count 20)
    (when (require 'orderless nil t)
      (setq completion-styles '(orderless))))
  (spacemacs/defer-until-after-user-config #'vertico-mode))
