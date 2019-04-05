
(setq github-packages-packages
      '(
        (evil-plugins :location (recipe :fetcher github :repo "tarao/evil-plugins"))
        (helm-fzf :location (recipe :fetcher github :repo "ofnhwx/helm-fzf"))
        ))

(defun github-packages/init-evil-plugins ()
  (use-package evil-little-word
    :after (evil))
  (use-package evil-textobj-between
    :after (evil)))

(defun github-packages/init-helm-fzf ()
  (use-package helm-fzf
    :defer t
    :init
    (set-variable 'helm-fzf-args nil)
    (spacemacs/set-leader-keys
      "fz" 'helm-fzf
      "pz" 'helm-fzf-project-root)))
