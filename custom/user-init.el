
;; custom-file を移動
(set-variable 'custom-file (expand-file-name "custom.el" e:private-directory))

;; フォントの設定
(progn
 (defun e:advice:spacemacs/set-default-font:after (&rest args)
   (set-fontset-font t 'unicode (font-spec :family e:font-name))
   (set-variable 'face-font-rescale-alist (list e:font-name e:font-rescale))
   (when (require 'eaw nil t)
     (eaw-fullwidth)))
 (advice-add 'spacemacs/set-default-font :after 'e:advice:spacemacs/set-default-font:after))

;; 「Viperize?」の確認をしない
(set-variable 'viper-mode nil)

;; Spacemacs が作成する環境変数を保存しておくファイル
(set-variable 'spacemacs-env-vars-file (expand-file-name "spacemacs.env" e:private-directory))

;; which-key の設定
(set-variable 'which-key-enable-extended-define-key t)
