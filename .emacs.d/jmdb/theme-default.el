(global-font-lock-mode 1) ;; allows syntax highlighting to work

(set-default-font "-apple-Monaco-medium-normal-normal-*-15-*-*-*-m-0-iso10646-1")
(add-to-list 'load-path "~/.emacs.d/lib/color-theme")

(require 'color-theme)
(color-theme-initialize)


(when (not (package-installed-p 'color-theme-solarized))
  (package-install 'color-theme-solarized))

;;(load-theme 'solarized-light t)

;; See here http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/

(add-to-list 'custom-theme-load-path "~/.emacs.d/jmdb/color-themes")
(load-theme 'minamin t) 





;;(color-theme-greiner)
;;(load-theme 'solarized-dark t)
;;(color-theme-minamin-light)
;;(color-theme-ld-dark)
;;(color-theme-taylor)
;;(color-theme-gnome)
;;(color-theme-billw)
;;(color-theme-charcoal-black)
;;(color-theme-greiner)

;;(color-theme-snowish)

;;(color-theme-gtk-ide)
;;(color-theme-lawrence)
;;(color-theme-standard)
;;(color-theme-taylor)
;;(color-theme-pok-wob)
