;;; vterm-theme.el

(defun vterm-theme-solarized-dark ()
  (interactive)
  (set-face-attribute 'vterm-color-black   nil :foreground "#073642" :background "#002b36")
  (set-face-attribute 'vterm-color-red     nil :foreground "#dc322f" :background "#cb4b16")
  (set-face-attribute 'vterm-color-green   nil :foreground "#859900" :background "#586e75")
  (set-face-attribute 'vterm-color-yellow  nil :foreground "#b58900" :background "#657b83")
  (set-face-attribute 'vterm-color-blue    nil :foreground "#268bd2" :background "#839496")
  (set-face-attribute 'vterm-color-magenta nil :foreground "#d33682" :background "#6c71c4")
  (set-face-attribute 'vterm-color-cyan    nil :foreground "#2aa198" :background "#93a1a1")
  (set-face-attribute 'vterm-color-white   nil :foreground "#eee8d5" :background "#fdf6e3"))

(defun vterm-theme-onehalf-dark ()
  (interactive)
  (set-face-attribute 'vterm-color-black   nil :foreground "#282c34" :background "#282c34")
  (set-face-attribute 'vterm-color-red     nil :foreground "#e06c75" :background "#e06c75")
  (set-face-attribute 'vterm-color-green   nil :foreground "#98c379" :background "#98c379")
  (set-face-attribute 'vterm-color-yellow  nil :foreground "#e5c07b" :background "#e5c07b")
  (set-face-attribute 'vterm-color-blue    nil :foreground "#61afef" :background "#61afef")
  (set-face-attribute 'vterm-color-magenta nil :foreground "#c678dd" :background "#c678dd")
  (set-face-attribute 'vterm-color-cyan    nil :foreground "#56b6c2" :background "#56b6c2")
  (set-face-attribute 'vterm-color-white   nil :foreground "#dcdfe4" :background "#dcdfe4"))

(provide 'vterm-theme)

;;; vterm-theme.el ends here
