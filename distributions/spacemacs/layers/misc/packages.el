;;; packages.el

(defvar misc-packages
  '(
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
    ox-reveal
    psysh
    visual-regexp
    vlf
    ))

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
    (set-variable 'cov-coverage-mode t)
    (prog1 "`simplecov' 用の設定"
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
                coverage))))))

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

(defun misc/init-ox-reveal ()
  (set-variable 'org-reveal-reveal-js-version 4)
  (set-variable 'org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(defun misc/init-psysh ())

(defun misc/init-vlf ())

(defun misc/init-visual-regexp ()
  (bind-key [remap query-replace] #'vr/query-replace))
