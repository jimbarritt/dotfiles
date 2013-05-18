(message "[jmdb] - Configuring emacs display.")

;;http://www.ibm.com/developerworks/linux/library/l-tip-prompt/

;; (require 'highline) ;; This is a newer version of hl-mode
;; (global-highline-mode)
;; (setq highline-priority 0)


(require 'rainbow-mode)
;;(add-hook 'css-mode-hook 'rainbow-mode)
;;(add-hook 'html-mode-hook 'rainbow-mode)

;;(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

;;(global-hl-line-mode)
(add-hook 'emacs-lisp-mode-hook 'hl-line-mode)



(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))
;; Color theme resources:
;; http://gnuemacscolorthemetest.googlecode.com/svn/html/index-java.html - shows pictures of them all

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
  (fringe-mode '(1 . 1))) ;; Hide the fringes (0 hide, 1 show both)


(when (not (package-installed-p 'rainbow-delimiters))
  (package-install 'rainbow-delimiters))

(global-rainbow-delimiters-mode)

(when (not (package-installed-p 'rainbow-mode))
  (package-install 'rainbow-mode))

(require 'rainbow-mode)
(rainbow-mode)
;;(setq-default truncate-lines t)
;; or
(setq-default global-visual-line-mode t)

(load "jmdb/windows.el")
(load "jmdb/header-line.el")
(load "jmdb/modeline.el")

(load "jmdb/theme-default.el")
;;(load "jmdb/theme-dark.el")
;;(load "jmdb/theme-dark-presentation.el")
;;(load "jmdb/theme-light.el")
;;(load "jmdb/theme-light-presentation.el")

(load "jmdb/scratch-message.el")

(defun theme-light ()
  (interactive)
  (load "jmdb/theme-light.el"))

(defun theme-dark ()
  (interactive)
  (load "jmdb/theme-dark.el"))


;;(setq ns-auto-hide-menu-bar t)
;;(set-frame-position (selected-frame) 0 -20)
;;(set-frame-position (selected-frame) 0 0)
;;(set-frame-height (selected-frame) 39)
(set-cursor-color "purple")
(message "[jmdb] - Display configured.")
