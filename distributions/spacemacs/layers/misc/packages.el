;;; packages.el

(defvar misc-packages
  '(
    apheleia
    atomic-chrome
    codic
    cov
    ddskk-posframe
    dired-filter
    dired-toggle-sudo
    elisp-demos
    flycheck-posframe
    grugru
    helpful
    highlight-indent-guides
    jinx
    leaf
    ob-typescript
    pacfiles-mode
    rails-routes
    separedit
    sqlite3
    visual-regexp
    vlf
    wakatime-mode
    ))

(defun misc/init-apheleia ()
  (use-package apheleia
    :spacediminish (apheleia-mode "")
    :defer (spacemacs/defer)
    :init
    (spacemacs/defer-until-after-user-config #'apheleia-global-mode)
    :config
    (defun apheleia-inhibit-whitout-prog-mode ()
      (not (derived-mode-p 'prog-mode)))
    (defun apheleia-inhibit-rubocop-excludes ()
      (or (s-ends-with? "/db/schema.rb" buffer-file-name)))
    (set-variable 'apheleia-inhibit-functions
                  '(apheleia-inhibit-whitout-prog-mode
                    apheleia-inhibit-rubocop-excludes))
    ;; formatters
    (setf (alist-get 'rubocop apheleia-formatters)
          '((if (e:bundle-exists "rubocop")
                '("bundle" "exec" "rubocop")
              "rubocop")
            file "--autocorrect" "--stderr" "--format" "quiet" "--fail-level" "fatal"))
    ;; mode-alist
    (setf (alist-get 'ruby-mode apheleia-mode-alist) '(rubocop))))

(defun misc/init-atomic-chrome ()
  (use-package atomic-chrome
    :defer (spacemacs/defer)
    :init
    (spacemacs/defer-until-after-user-config #'atomic-chrome-start-server)))

(defun misc/init-codic ()
  (use-package codic
    :no-require t))

(defun misc/init-cov ()
  (use-package cov
    :defer (spacemacs/defer)
    :spacediminish (cov-mode "☂")
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

(defun misc/init-flycheck-posframe ()
  (use-package flycheck-posframe
    :hook (flycheck-mode . flycheck-posframe-mode)))

(defun misc/init-grugru ()
  (use-package grugru
    :defer t
    :init
    (spacemacs/set-leader-keys "xgg" #'grugru)))

(defun misc/init-helpful ()
  (use-package helpful
    :defer (spacemacs/defer)
    :init
    (spacemacs/declare-prefix "hh" "helpful")
    (spacemacs/set-leader-keys
      "hhc" 'helpful-callable
      "hhf" 'helpful-function
      "hhh" 'helpful-at-point
      "hhi" 'helpful-command
      "hhk" 'helpful-key
      "hhm" 'helpful-macro
      "hhs" 'helpful-symbol
      "hhv" 'helpful-variable)
    :config
    (evil-define-key 'normal helpful-mode-map (kbd "gr") 'helpful-update)
    (evil-define-key 'normal helpful-mode-map (kbd "q") 'quit-window)
    (push '(helpful-mode :dedicated t :stick t) popwin:special-display-config)))

(defun misc/init-highlight-indent-guides ()
  (use-package highlight-indent-guides
    :hook ((haml-mode . spacemacs/toggle-highlight-indent-guides-mode-on)
           (yaml-mode . spacemacs/toggle-highlight-indent-guides-mode-on))
    :config
    (e:variable! highlight-indent-guides-method 'character)
    (e:variable! highlight-indent-guides-responsive 'top)
    (spacemacs|add-toggle highlight-indent-guides-mode
      :status highlight-indent-guides-mode
      :on  (highlight-indent-guides-mode 1)
      :off (highlight-indent-guides-mode 0))))

(defun misc/init-jinx ()
  (use-package jinx
    :hook (prog-mode . jinx-mode)
    :spacediminish (jinx-mode "")
    :config
    ;; libenchant, aspell aspell-en
    (set-variable 'jinx-languages "en_US")))

(defun misc/init-leaf ()
  (use-package leaf
    :no-require t))

(defun misc/init-ob-typescript ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-typescript
      :init
      (add-to-list 'org-babel-load-languages '(typescript . t)))))

(defun misc/init-pacfiles-mode ()
  (use-package pacfiles-mode
    :no-require t))

(defun misc/init-rails-routes ()
  (use-package rails-routes
    :defer (spacemacs/defer)
    :config
    (set-variable 'rails-routes-cache-path (expand-file-name "rails-routes" spacemacs-cache-directory))))

(defun misc/init-separedit ()
  (use-package separedit
    :bind (:map prog-mode-map
                ("C-c '" . separedit))
    :config
    (set-variable 'separedit-preserve-string-indentation t)))

(defun misc/init-sqlite3 ()
  (use-package sqlite3
    :no-require t))

(defun misc/init-visual-regexp ()
  (use-package visual-regexp
    :bind (([remap query-replace] . vr/query-replace))))

(defun misc/init-vlf ()
  (use-package vlf
    :no-require t))

(defun misc/init-wakatime-mode ()
  (use-package wakatime-mode
    :defer (spacemacs/defer)
    :spacediminish (wakatime-mode "")
    :commands (global-wakatime-mode spacemacs/wakatime-dashboard)
    :init
    (spacemacs/set-leader-keys
      "aW" 'spacemacs/wakatime-dashboard)
    :config
    (defun spacemacs/wakatime-dashboard ()
      (interactive)
      (browse-url "https://wakatime.com/dashboard"))))
