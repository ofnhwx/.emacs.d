;;; user-config-org.el


(leaf org-support
  :config
  (defun org-support/archive-file ()
    (f-expand (format-time-string "archives/%Y.org") org-directory))
  (defun org-support/tasks-file ()
    (f-expand "tasks.org" org-directory))
  (defun org-support/note-file ()
    (f-expand "note.org" org-directory))
  (defun org-support/popwin:tasks ()
    (interactive)
    (popwin:popup-buffer (find-file-noselect (org-support/tasks-file)) :height 30 :dedicated t :stick t))
  (defun org-support/popwin:note ()
    (interactive)
    (popwin:popup-buffer (find-file-noselect (org-support/note-file)) :height 30 :dedicated t :stick t)))

(leaf org
  :config
  (e:variable! org-directory (f-expand "~/org/"))
  (e:variable! org-default-notes-file (org-support/note-file))
  (e:variable! org-startup-folded nil)
  (e:variable! org-startup-indented t)
  (e:variable! org-tags-column 0)
  (e:variable! org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                                   (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)"))))

(leaf org-agenda
  :defer-config
  (e:variable! org-agenda-current-time-string "← now")
  (e:variable! org-agenda-entry-text-leaders (s-concat (s-repeat 25 " ") "│ "))
  (e:variable! org-agenda-entry-text-maxlines 20)
  (e:variable! org-agenda-files (list (org-support/note-file)
                                      (org-support/tasks-file)
                                      (f-parent (org-support/archive-file))))
  (e:variable! org-agenda-span 28)
  (e:variable! org-agenda-time-grid '((daily today require-timed)
                                      (800 1000 1200 1400 1600 1800 2000)
                                      "      "
                                      "────────────────")))

(leaf org-capture
  :defer-config
  (e:variable! org-capture-templates
               `(("t" "Todo" entry (file+headline ,(org-support/tasks-file) "Inbox") "%[~/org/template/todo.org]" :immediate-finish t))))

(leaf org-faces
  :defer-config
  (e:variable! org-todo-keyword-faces '(("TODO" . org-warning)
                                        ("WAITING" . org-done)
                                        ("HOLD" . org-done)))
  (set-face-attribute 'org-todo nil :foreground "#00ff00")
  (set-face-attribute 'org-done nil :foreground "#696969")
  (set-face-attribute 'org-headline-done nil :foreground "#696969")
  (set-face-attribute 'org-headline-todo nil :foreground "#00ff00")
  (set-face-attribute 'org-level-1 nil :height 1.0)
  (set-face-attribute 'org-level-2 nil :height 1.0)
  (set-face-attribute 'org-level-3 nil :height 1.0))

(leaf org-indent
  :defer-config
  (e:variable! org-indent-indentation-per-level 2)
  (e:variable! org-indent-mode-turns-on-hiding-stars nil))

(leaf org-refile
  :defer-config
  (e:variable! org-refile-targets '((org-support/tasks-file   :level . 1)
                                    (org-support/archive-file :level . 1)))
  (e:variable! org-refine-use-outline-path 'file))

(leaf org-clock
  :init
  (spacemacs/defer-until-after-user-config #'org-clock-persistence-insinuate)
  :defer-config
  (e:variable! org-clock-persist t))

(leaf org-src
  :config
  (e:variable! org-edit-src-content-indentation 0)
  (e:variable! org-src-window-setup 'split-window-below))

(provide 'user-config-org)

;;; user-config-org.el ends here
