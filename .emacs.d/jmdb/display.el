(message "[jmdb] - Configuring emacs display.")

(tool-bar-mode 0) ;; hide the toolbar

(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no"

(show-paren-mode) ;; Highlight matching parentheses

(setq visible-bell 1) ;;To stop it making a bell noise...
(setq ring-bell-function 'ignore)

(when (fboundp 'fringe-mode)
  (fringe-mode '(1 . 0))) ;; Hide the fringes (0 hide, 1 show both)


(load "jmdb/windows.el")
(load "jmdb/theme.el")

(message "[jmdb] - Display configured.")
