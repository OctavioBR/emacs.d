(package-initialize)

(global-linum-mode t)      ; show line numbers
(tool-bar-mode -1)         ; remove tool bar
(menu-bar-mode -1)         ; remove menu bar
(setq-default tab-width 4) ; tab with 4 spaces

; Default window size
(add-to-list 'default-frame-alist '(width  . 125))
(add-to-list 'default-frame-alist '(height . 43))

; don't save backup files next to original, put them in /tmp
(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

; Functions to move line up and down
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
;; TODO
