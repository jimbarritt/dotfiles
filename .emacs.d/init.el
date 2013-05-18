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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("78504a2202165362c550a057e0a3824b5874b1289daef4de5ceb0a4dbfb73bcc" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "07825f19e342c764788732c025fc985ba457bb0b8eefca67d6369d7316ac46d0" "b7898654628d353c60e729f2c029687d798514fbbcedf78e6acc112a335f2fea" "27b53b2085c977a8919f25a3a76e013ef443362d887d52eaa7121e6f92434972" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
