
(defvar e:mode-line-foreground (face-foreground 'mode-line))
(defvar e:mode-line-background (face-background 'mode-line))

(defun e:auto-reset-mode-line-color (fn face &rest args)
  (apply fn face args)
  (when (eq face 'mode-line)
    (let ((inhibit-quit t))
      (sit-for 3)
      (funcall fn 'mode-line nil
               :foreground e:mode-line-foreground
               :background e:mode-line-background))))

(defun e:auto-reset-mode-line-color-on ()
  (interactive)
  (add-hook 'set-face-attribute :around #'e:auto-reset-mode-line-color-on))

(defun e:auto-reset-mode-line-color-off ()
  (interactive)
  (remove-hook 'set-face-attribute #'e:auto-reset-mode-line-color-on))

(provide 'e:auto-reset-mode-line-color)
