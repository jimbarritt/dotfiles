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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "159bb8f86836ea30261ece64ac695dc490e871d57107016c09f286146f0dae64" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "Green"))))
 '(diff-removed ((t (:foreground "Red"))))
 '(mode-line ((t (:foreground "#777777" :background "#222222" :box nil))))
 '(mode-line-inactive ((t (:foreground "#555555" :background "#222222" :box nil)))))
