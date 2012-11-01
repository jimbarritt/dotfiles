;; Jims Emacs customisation entry point

(message "[jmdb] - Emacs Customisation Start.")

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/lib")

(load "jmdb/display.el")
(load "jmdb/keyboard.el")


(message "[jmdb] - Emacs Customisation End.")
