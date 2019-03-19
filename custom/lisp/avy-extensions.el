
(eval-when-compile
  (require 'avy)
  (require 'evil))



(defun e:avy-goto-char-1 (char &optional arg beg end)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg
                     nil nil))
  (when (eq char ?)
    (setq char ?\n))
  (avy-with e:avy-goto-char-1
    (avy-jump
     (regexp-quote (string char))
     :window-flip arg
     :beg beg
     :end end)))

(defun e:avy-goto-char-1-below (char &optional arg)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with e:avy-goto-char-1-below
    (e:avy-goto-char-1
     char arg
     (point) (window-end (selected-window) t))))

(defun e:avy-goto-char-1-above (char &optional arg)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with e:avy-goto-char-1-above
    (e:avy-goto-char-1
     char arg
     (window-start) (point))))



(evil-define-avy-motion e:avy-goto-char-1 inclusive)
(evil-define-avy-motion e:avy-goto-char-1-below inclusive)
(evil-define-avy-motion e:avy-goto-char-1-above inclusive)



(provide 'avy-extensions)
