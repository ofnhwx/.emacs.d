;;; packages.el

(defvar misc-packages
  '(
    affe
    atomic-chrome
    beacon
    company-org-block
    company-prescient
    company-tabnine
    company-try-hard
    cov
    ddskk-posframe
    dired-filter
    dired-toggle-sudo
    elisp-demos
    grugru
    helm-icons
    helpful
    leaf
    magit-libgit
    modus-themes
    ob-typescript
    psysh
    visual-regexp
    vlf
    (codic :toggle (getenv "EMACS_CODIC_API_TOKEN"))
    (wakatime-mode :toggle (getenv "EMACS_WAKATIME_API_KEY"))
    ))

(defun misc/init-affe ()
  (use-package affe
    :defer (spacemacs/defer)
    :init
    (spacemacs/set-leader-keys
      "fz" 'affe-find
      "fg" 'affe-grep)
    :config
    (set-variable 'affe-find-command (or (executable-find "fd") affe-find-command))
    (set-variable 'affe-regexp-function 'orderless-pattern-compiler)
    (set-variable 'affe-highlight-function 'orderless--highlight)))

(defun misc/init-atomic-chrome ()
  (use-package atomic-chrome
    :defer (spacemacs/defer)
    :init
    (spacemacs/defer-until-after-user-config #'atomic-chrome-start-server)))

(defun misc/init-beacon ()
  (use-package beacon
    :defer (spacemacs/defer)
    :spacediminish beacon-mode
    :init
    (spacemacs/defer-until-after-user-config #'beacon-mode)))

(defun misc/init-codic ()
  (use-package codic
    :defer (spacemacs/defer)
    :config
    (set-variable 'codic-api-token (getenv "EMACS_CODIC_API_TOKEN"))))

(defun misc/init-company-org-block ()
  (use-package company-org-block
    :after (company org)
    :config
    (set-variable 'company-org-block-edit-style 'inline)
    (with-no-warnings (spacemacs|add-company-backends :backends company-org-block :modes org-mode))))

(defun misc/init-company-prescient ()
  (use-package company-prescient
    :after (company)
    :config
    (company-prescient-mode 1)))

(defun misc/init-company-tabnine ()
  (use-package company-tabnine
    :defer (spacemacs/defer)
    :config
    (set-variable 'company-tabnine-binaries-folder
                  (expand-file-name "tabnine" spacemacs-cache-directory))))

(defun misc/init-company-try-hard ()
  (use-package company-try-hard
    :bind (("C-z" . company-try-hard)
           :map company-active-map
           ("C-z" . company-try-hard))))

(defun misc/init-cov ()
  (use-package cov
    :defer (spacemacs/defer)
    :spacediminish (cov-mode " ☂" " COV")
    :config
    (set-variable 'cov-coverage-file-paths '(cov--locate-simplecov))
    (set-variable 'cov-coverage-mode t)))

(defun misc/init-ddskk-posframe ()
  (use-package ddskk-posframe
    :spacediminish ddskk-posframe-mode
    :after (skk)
    :config
    (ddskk-posframe-mode 1)))

(defun misc/init-dired-filter ()
  (use-package dired-filter
    :hook (dired-mode . dired-filter-mode)))

(defun misc/init-dired-toggle-sudo ()
  (use-package dired-toggle-sudo
    :no-require t))

(defun misc/init-elisp-demos ()
  (use-package elisp-demos
    :defer (spacemacs/defer)
    :init
    (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    (advice-add 'helpful-update      :after #'elisp-demos-advice-helpful-update)))

(defun misc/init-grugru ()
  (use-package grugru
    :defer t
    :init
    (spacemacs/set-leader-keys "xgg" #'grugru)))

(defun misc/init-helm-icons ()
  (use-package helm-icons
    :defer (spacemacs/defer)
    :init
    (spacemacs/defer-until-after-user-config #'helm-icons-enable)
    :config
    (set-variable 'helm-icons-mode->icon nil)))

(defun misc/init-helpful ()
  (use-package helpful
    :defer (spacemacs/defer)
    :init
    (spacemacs/declare-prefix "hdd" "helpful")
    (spacemacs/set-leader-keys
      "hddc" 'helpful-callable
      "hddd" 'helpful-at-point
      "hddf" 'helpful-function
      "hddi" 'helpful-command
      "hddk" 'helpful-key
      "hddm" 'helpful-macro
      "hdds" 'helpful-symbol
      "hddv" 'helpful-variable)
    :config
    (evil-define-key 'normal helpful-mode-map (kbd "gr") 'helpful-update)
    (evil-define-key 'normal helpful-mode-map (kbd "q") 'quit-window)))

(defun misc/init-leaf ()
  (use-package leaf
    :no-require t))

(defun misc/init-magit-libgit ()
  (use-package magit-libgit
    :after (magit)
    :config
    (libgit-load)))

(defun misc/init-modus-themes ()
  (use-package modus-themes
    :no-require t))

(defun misc/init-ob-typescript ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-typescript
      :init
      (add-to-list 'org-babel-load-languages '(typescript . t)))))

(defun misc/init-psysh ()
  (use-package psysh
    :no-require t))

(defun misc/init-vlf ()
  (use-package vlf
    :no-require t))

(defun misc/init-visual-regexp ()
  (use-package visual-regexp
    :bind (([remap query-replace] . vr/query-replace))))

(defun misc/init-wakatime-mode ()
  (use-package wakatime-mode
    :defer (spacemacs/defer)
    :spacediminish (wakatime-mode " " " [T]")
    :commands (global-wakatime-mode)
    :init
    (spacemacs/defer-until-after-user-config #'global-wakatime-mode)
    :config
    (set-variable 'wakatime-api-key (getenv "EMACS_WAKATIME_API_KEY"))
    (defun spacemacs/wakatime-dashboard ()
      (interactive)
      (browse-url "https://wakatime.com/dashboard"))
    (spacemacs/set-leader-keys
      "aW" 'spacemacs/wakatime-dashboard)))
