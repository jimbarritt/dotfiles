;; Jims Emacs customisation entry point

(message "[jmdb] - Emacs Customisation Start.")

(server-start)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp") ;; magit likes to live in here

(setq exec-path (append exec-path '("/usr/local/bin")))

(load "jmdb/display.el")
(load "jmdb/keyboard.el")


(when (> emacs-major-version 23)
        (require 'package)
        (package-initialize)
        (add-to-list 'package-archives 
                     '("melpa" . "http://melpa.milkbox.net/packages/")
		     '("marmalade" . "http://marmalade-repo.org/packages/")))



(load "jmdb/behaviour.el")
(load "jmdb/code.el")
(load "jmdb/uncatalogued.el")



(message "[jmdb] - Emacs Customisation End.")
