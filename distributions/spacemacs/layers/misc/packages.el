;;; packages.el

(defvar misc-packages
  '(
    affe
    atomic-chrome
    beacon
    codic
    company-org-block
    company-prescient
    company-tabnine
    company-try-hard
    cov
    deadgrep
    dired-filter
    dired-toggle-sudo
    elisp-demos
    evil-owl
    foreman-mode
    good-scroll
    grugru
    helpful
    leaf
    magit-libgit
    ob-typescript
    orderless
    ox-reveal
    psysh
    visual-regexp
    vlf
    ))

(defun misc/init-affe ()
  (with-eval-after-load 'affe
    (when (require 'orderless nil t)
      (set-variable 'affe-regexp-function 'orderless-pattern-compiler)
      (set-variable 'affe-highlight-function 'orderless--highlight)))
  (spacemacs/set-leader-keys
    "fz" 'affe-find
    "pz" 'affe-find-in-project))

(defun misc/init-atomic-chrome ()
  (spacemacs/defer-until-after-user-config #'atomic-chrome-start-server))

(defun misc/init-beacon ()
  (with-eval-after-load 'beacon
    (spacemacs|hide-lighter beacon-mode))
  (spacemacs/defer-until-after-user-config #'beacon-mode))

(defun misc/init-codic ())

(defun misc/init-company-org-block ()
  (with-no-warnings (spacemacs|add-company-backends :backends company-org-block :modes org-mode)))

(defun misc/init-company-prescient ()
  (with-eval-after-load 'company
    (company-prescient-mode 1)))

(defun misc/init-company-tabnine ()
  (with-eval-after-load 'company-tabnine
    (set-variable 'company-tabnine-binaries-folder
                  (expand-file-name "tabnine" spacemacs-cache-directory))))

(defun misc/init-company-try-hard ()
  (bind-keys :package company-try-hard
             ("C-z" . company-try-hard)
             :map company-active-map
             ("C-z" . company-try-hard)))

(defun misc/init-cov ()
  (with-eval-after-load 'cov
    (spacemacs|diminish cov-mode " ☂" " COV")
    (set-variable 'cov-coverage-file-paths '(cov--locate-simplecov))
    (set-variable 'cov-coverage-mode t)))

(defun misc/init-deadgrep ())

(defun misc/init-dired-filter ()
  (add-hook 'dired-mode-hook #'dired-filter-mode))

(defun misc/init-dired-toggle-sudo ())

(defun misc/init-elisp-demos ()
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update      :after #'elisp-demos-advice-helpful-update))

(defun misc/init-evil-owl ()
  (with-eval-after-load 'evil-owl
    (spacemacs|hide-lighter evil-owl-mode))
  (spacemacs/defer-until-after-user-config #'evil-owl-mode))

(defun misc/init-foreman-mode ()
  (eval-when-compile
    (defvar foreman-mode-map)
    (declare-function foreman-kill-proc 'foreman-mode)
    (declare-function foreman-restart   'foreman-mode)
    (declare-function foreman-stop      'foreman-mode))
  (spacemacs/set-leader-keys "atf" #'foreman)
  (bind-keys :package foreman-mode
             :map foreman-mode-map
             ("R" . foreman-restart)
             ("S" . foreman-start)
             ("X" . foreman-stop)
             ("x" . foreman-kill-proc)))

(defun misc/init-good-scroll ()
  (spacemacs/defer-until-after-user-config #'good-scroll-mode))

(defun misc/init-grugru ()
  (spacemacs/set-leader-keys "xgg" #'grugru))

(defun misc/init-helpful ()
  (with-eval-after-load 'helpful
    (evil-define-key 'normal helpful-mode-map (kbd "gr") 'helpful-update)
    (evil-define-key 'normal helpful-mode-map (kbd "q") 'quit-window))
  (spacemacs/declare-prefix "hdd" "helpful")
  (spacemacs/set-leader-keys
    "hddc" 'helpful-callable
    "hddd" 'helpful-at-point
    "hddf" 'helpful-function
    "hddi" 'helpful-command
    "hddk" 'helpful-key
    "hddm" 'helpful-macro
    "hdds" 'helpful-symbol
    "hddv" 'helpful-variable))

(defun misc/init-leaf ())

(defun misc/init-magit-libgit ()
  (with-eval-after-load 'magit
    (libgit-load)))

(defun misc/init-ob-typescript ()
  (spacemacs|use-package-add-hook org
    :post-config
    (add-to-list 'org-babel-load-languages '(typescript . t))))

(defun misc/init-orderless ())

(defun misc/init-ox-reveal ()
  (set-variable 'org-reveal-reveal-js-version 4)
  (set-variable 'org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(defun misc/init-psysh ())

(defun misc/init-vlf ())

(defun misc/init-visual-regexp ()
  (bind-key [remap query-replace] #'vr/query-replace))
