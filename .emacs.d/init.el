;; Jims Emacs customisation entry point

(message "[jmdb] - Emacs Customisation Start.")

(server-start)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/lib")

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp") ;; magit likes to live in here

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq exec-path (append exec-path '("/usr/local/bin")))

(when (> emacs-major-version 23)
        (require 'package)
        (package-initialize)
        (add-to-list 'package-archives 
                     '("melpa" . "http://melpa.milkbox.net/packages/")
		     '("marmalade" . "http://marmalade-repo.org/packages/")))

(load "jmdb/keyboard.el")
(load "jmdb/display.el")


(load "jmdb/behaviour.el")
(load "jmdb/ido-conf.el")
(load "jmdb/code.el")
(load "jmdb/org-conf.el")
(load "jmdb/rcirc.el")



(message "[jmdb] - Emacs Customisation End.")
(put 'erase-buffer 'disabled nil)
