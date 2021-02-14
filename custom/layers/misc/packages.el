;;; packages.el

(defvar misc-packages
  '(
    atomic-chrome
    beacon
    codic
    color-identifiers-mode
    company-tabnine
    company-try-hard
    deadgrep
    dired-filter
    dired-toggle-sudo
    elisp-demos
    evil-owl
    foreman-mode
    grugru
    helpful
    leaf
    magit-libgit
    ox-reveal
    psysh
    rainbow-mode
    visual-regexp
    vlf
    ))

(defun misc/init-atomic-chrome ()
  (use-package atomic-chrome
    :defer t
    :config
    (spacemacs/defer-until-after-user-config #'atomic-chrome-start-server)))

(defun misc/init-beacon ()
  (use-package beacon
    :config
    (spacemacs|diminish beacon-mode)
    (beacon-mode 1)))

(defun misc/init-codic ()
  (use-package codic
    :defer t
    :config
    (set-variable 'codic-api-token (e:auth-source-get 'token :host "codic"))))

(defun misc/init-color-identifiers-mode ()
  (use-package color-identifiers-mode
    :config
    (spacemacs|diminish color-identifiers-mode)
    (global-color-identifiers-mode 1)))

(defun misc/init-company-tabnine ()
  (use-package company-tabnine
    :defer t
    :init
    (set-variable 'company-tabnine-binaries-folder
                  (expand-file-name "tabnine" spacemacs-cache-directory))))

(defun misc/init-company-try-hard ()
  (use-package company-try-hard
    :bind (("C-z" . company-try-hard)
           :map company-active-map
           ("C-z" . company-try-hard))))

(defun misc/init-deadgrep ()
  (use-package deadgrep
    :defer t))

(defun misc/init-dired-filter ()
  (use-package dired-filter
    :hook (dired-mode . dired-filter-mode)))

(defun misc/init-dired-toggle-sudo ()
  (use-package dired-toggle-sudo
    :defer t))

(defun misc/init-elisp-demos ()
  (use-package elisp-demos
    :defer t
    :init
    (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    (advice-add 'helpful-update      :after #'elisp-demos-advice-helpful-update)))

(defun misc/init-evil-owl ()
  (use-package evil-owl
    :config
    (spacemacs|diminish evil-owl-mode)
    (evil-owl-mode 1)))

(defun misc/init-foreman-mode ()
  (use-package foreman-mode
    :bind (:map foreman-mode-map
                ("R" . foreman-restart)
                ("S" . foreman-start)
                ("X" . foreman-stop)
                ("x" . foreman-kill-proc))
    :init
    (spacemacs/set-leader-keys "atf" #'foreman)))

(defun misc/init-grugru ()
  (use-package grugru
    :defer t
    :init
    (spacemacs/set-leader-keys "xx" #'grugru)))

(defun misc/init-helpful ()
  (use-package helpful
    :defer t
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
      "hddv" 'helpful-variable)))

(defun misc/init-leaf ()
  (use-package leaf
    :defer t))

(defun misc/init-magit-libgit ()
  (use-package magit-libgit
    :defer t))

(defun misc/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :init
    (set-variable 'org-reveal-reveal-js-version 4)
    (set-variable 'org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")))

(defun misc/init-psysh ()
  (use-package psysh
    :defer t))

(defun misc/init-rainbow-mode ()
  (use-package rainbow-mode
    :hook (prog-mode . rainbow-mode)
    :config
    (spacemacs|diminish rainbow-mode)))

(defun misc/init-vlf ()
  (use-package vlf
    :defer t))

(defun misc/init-visual-regexp ()
  (use-package visual-regexp
    :bind (([remap query-replace] . vr/query-replace))))
