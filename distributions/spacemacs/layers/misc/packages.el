;;; packages.el

(defvar misc-packages
  '(
    atomic-chrome
    beacon
    codic
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
    grugru
    helpful
    leaf
    magit-libgit
    ob-typescript
    ox-reveal
    psysh
    visual-regexp
    vlf
    ))

(defun misc/init-atomic-chrome ()
  (use-package atomic-chrome
    :config
    (spacemacs/defer-until-after-user-config #'atomic-chrome-start-server)))

(defun misc/init-beacon ()
  (use-package beacon
    :spacediminish beacon-mode
    :config
    (beacon-mode 1)))

(defun misc/init-codic ()
  (use-package codic
    :no-require t))

(defun misc/init-company-prescient ()
  (use-package company-prescient
    :after company
    :config
    (company-prescient-mode 1)))

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

(defun misc/init-cov ()
  (use-package cov
    :defer t
    :spacediminish (cov-mode " ☂" " COV")
    :init
    (set-variable 'cov-coverage-file-paths '(cov--locate-simplecov))
    (set-variable 'cov-coverage-mode t)
    :config
    (defun cov--locate-simplecov (file-dir file-name)
      (let ((dir (kllib:project-root file-dir)))
        (when dir
          (cons (f-expand "coverage/.resultset.json" dir) 'simplecov))))
    (defun cov--simplecov-parse ()
      (let* ((contents (buffer-string))
             (coverage (let-alist (json-parse-string contents :object-type 'alist :array-type 'list)
                         .RSpec.coverage))
             (project-root (f-expand (kllib:project-root cov-coverage-file))))
        (-map (lambda (item)
                (let ((file (symbol-name (first item)))
                      (list (cdadr item)))
                  (cons (s-replace project-root "" file)
                        (->> list
                          (--map-indexed (list (1+ it-index) it))
                          (--reject (eq (second it) :null))))))
              coverage)))))

(defun misc/init-deadgrep ()
  (use-package deadgrep
    :no-require))

(defun misc/init-dired-filter ()
  (use-package dired-filter
    :hook (dired-mode . dired-filter-mode)))

(defun misc/init-dired-toggle-sudo ()
  (use-package dired-toggle-sudo
    :no-require t))

(defun misc/init-elisp-demos ()
  (use-package elisp-demos
    :defer t
    :init
    (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    (advice-add 'helpful-update      :after #'elisp-demos-advice-helpful-update)))

(defun misc/init-evil-owl ()
  (use-package evil-owl
    :spacediminish evil-owl-mode
    :config
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
    :no-require t))

(defun misc/init-magit-libgit ()
  (use-package magit-libgit
    :after (magit)
    :config
    (libgit-load)))

(defun misc/init-ob-typescript ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-typescript
      :init
      (add-to-list 'org-babel-load-languages '(typescript . t)))))

(defun misc/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :init
    (set-variable 'org-reveal-reveal-js-version 4)
    (set-variable 'org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")))

(defun misc/init-psysh ()
  (use-package psysh
    :no-require t))

(defun misc/init-vlf ()
  (use-package vlf
    :no-require t))

(defun misc/init-visual-regexp ()
  (use-package visual-regexp
    :bind (([remap query-replace] . vr/query-replace))))
