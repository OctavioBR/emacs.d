;; https://github.com/purcell/emacs.d/blob/master/README.md#changing-themes-and-adding-your-own-customization

(global-linum-mode t)

(setq-default tab-width 4)

(setq create-lockfiles nil)

;; (require 'editorconfig)
;; (editorconfig-mode 1)

;; Applies goimports (if executabvle available, otherwise fmt) before saving go code
(with-eval-after-load 'go-mode
  (add-hook 'before-save-hook 'gofmt-before-save)
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports))))

(add-to-list 'default-frame-alist '(width  . 125))
(add-to-list 'default-frame-alist '(height . 43))

(provide 'init-local)
