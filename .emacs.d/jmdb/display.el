(message "[jmdb] - Configuring emacs display.")

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq inhibit-startup-message t)
(tool-bar-mode -1) ;; hide the toolbar
;; Only want to do this if running in the terminal (menu-bar-mode -1) ;; hide the menubar
(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no"
(show-paren-mode) ;; Highlight matching parentheses
(setq visible-bell 1) ;;To stop it making a bell noise...
(setq ring-bell-function 'ignore)

(when (fboundp 'fringe-mode)
  (fringe-mode '(1 . 0))) ;; Hide the fringes (0 hide, 1 show both)

(when (not (package-installed-p 'rainbow-delimiters))
  (package-install 'rainbow-delimiters))

(when (not (package-installed-p 'rainbow-mode))
  (package-install 'rainbow-mode))

;;(setq-default truncate-lines t)
;; or
(setq-default global-visual-line-mode t)

(load "jmdb/windows.el")
(load "jmdb/modeline.el")
(load "jmdb/theme.el")
(load "jmdb/scratch-message.el")



;;(setq ns-auto-hide-menu-bar t)
;;(set-frame-position (selected-frame) 0 -20)
;;(set-frame-position (selected-frame) 0 0)
;;(set-frame-height (selected-frame) 39)

(message "[jmdb] - Display configured.")
