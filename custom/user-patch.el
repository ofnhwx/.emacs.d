
(use-package vagrant-tramp
  :defer t
  :config
  (let ((version (pkg-info-package-version "vagrant-tramp"))
        (target '(20160427 2332)))
    (if (equal version target)
        (defun vagrant-tramp--all-boxes ()
          "List of VMs per `vagrant global-status` as alists."
          (let* ((status-cmd "vagrant global-status --machine-readable")
                 (status-raw (shell-command-to-string status-cmd))
                 (status-lines (-drop 7 (split-string status-raw "\n")))
                 (status-data-raw (--map (mapconcat 'identity
                                                    (-drop 4 (split-string it ",")) ",")
                                         status-lines))
                 (status-data (--map (replace-regexp-in-string " " "" it) status-data-raw))
                 (status-groups (-butlast (-split-on "" status-data)))
                 (vm-attrs '(id name provider state dir)))
            (--map (-zip vm-attrs it) status-groups)))
      (spacemacs-buffer/warning "`vagrant-tramp' was updated."))))

(use-package avy-migemo
  :defer t
  :config
  (let ((version (pkg-info-package-version "avy-migemo"))
        (target '(20180716 1455)))
    (if (equal version target)
        (progn
          (defun e:avy--generic-jump:filter-args (args)
            (if (= (length args) 4)
                args
              (e:remove-nth 2 args)))
          (advice-add 'avy--generic-jump :filter-args 'e:avy--generic-jump:filter-args))
      (spacemacs-buffer/warning "`avy-migemo' was updated."))))
