;; [TIP] Use "M-x eval-buffer" to avoid reopening emacs every time
(require 'package)

;; Configure Melpa
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
;; To add stable channel
;; (add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)

(package-initialize)

;; Editor UI
(global-linum-mode t)      ; show line numbers
(tool-bar-mode -1)         ; remove tool bar
(menu-bar-mode -1)         ; remove menu bar
(setq-default tab-width 4) ; tab with 4 spaces

;; Default window size
(add-to-list 'default-frame-alist '(width  . 125))
(add-to-list 'default-frame-alist '(height . 43))

;; Don't save backup files next to original, put them in /tmp
(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;; Functions to move line up and down
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)

;; Dracula Theme
(load-theme 'dracula t)

;; Hack Font
;; (set-face-attribute 'default t :font "Hack 14")

;; Open buffer menu in same window (keybind)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-unset-key (kbd "<C-down-mouse-1>")) ; disable mouse buffer menu

;; Applies goimports (if executable available, otherwise fmt) before saving go code
(with-eval-after-load 'go-mode
  (add-hook 'before-save-hook 'gofmt-before-save)
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports))))

;; Origami-mode keybindings
(global-set-key (kbd "C-c f") 'origami-recursively-toggle-node)
(global-set-key (kbd "C-c v") 'origami-show-only-node)
(global-set-key (kbd "C-c F") 'origami-toggle-all-nodes)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (flycheck sudo-edit origami go-mode dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282a36" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "SRC" :family "Hack")))))
