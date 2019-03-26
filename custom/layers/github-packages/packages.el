
(setq github-packages-packages
      '(
        (helm-fzf :location (recipe :fetcher github :repo "ofnhwx/helm-fzf"))
        ))

(defun github-packages/init-helm-fzf ()
  (use-package helm-fzf
    :defer t
    :init
    (set-variable 'helm-fzf-args nil)
    (spacemacs/set-leader-keys
      "fz" 'helm-fzf
      "pz" 'helm-fzf-project-root)))
