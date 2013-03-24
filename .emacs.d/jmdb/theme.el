;;; Setup of color theme

(load-theme 'ir-black-evolv 0)

;; (when (not (package-installed-p 'cyberpunk-theme))
;;   (package-install 'cyberpunk-theme))

(when (not (package-installed-p 'ir-black-theme))
  (package-install 'ir-black-theme))

;;(require 'ir-black-theme)

(when (not (package-installed-p 'zenburn-theme))
  (package-install 'zenburn-theme))

;;(require 'zenburn-theme)



(global-font-lock-mode 1) ;; allows syntax highlighting to work

;; To change the font size interactively per buffer:
;; Up:    C-x C-+, C-x C-=
;; Down:  C-x C--
;; Reset: C-x C-0

(set-default-font "-apple-Inconsolata-medium-normal-normal--16-180-75-75-m-0-iso10646-1")
;;(set-default-font "-apple-Inconsolata-medium-normal-normal--17-180-75-75-m-0-iso10646-1")
;;(set-default-font "-apple-Courier_New-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")
;;(set-default-font "-apple-Monaco-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
;;(add-to-list 'default-frame-alist '(font . "-apple-Monaco-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1"))

;; In cocoa emacs, you can set the font in Options->Change default font.
;;(set-default-font "-apple-Consolas-medium-normal-normal--18-180-75-75-m-0-iso10646-1")
;;(set-default-font "-apple-Inconsolata-medium-normal-normal--20-180-75-75-m-0-iso10646-1")
;;(set-default-font "-apple-Courier_New-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
;;(set-default-font "-apple-Inconsolata-medium-normal-normal--24-180-75-75-m-0-iso10646-1")
;;(set-default-font "-ibm-courier-medium-r-normal--0-0-0-0-m-0-iso8859-1")


;; Color themes
(add-to-list 'load-path "~/.emacs.d/lib/color-theme")
(require 'color-theme)
;;(color-theme-initialize)

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



;; Jims' Customisations:

;; Diff mode:
(custom-set-faces
 '(diff-added ((t (:foreground "Green"))) 'now)
 '(diff-removed ((t (:foreground "Red"))) 'now))

;; dif-hl:

(custom-set-faces
 '(diff-hl-insert ((t (:foreground "Green4" :background "#224422"))) 'now))

;; The modeline:

(custom-set-faces
   '(mode-line ((t (:foreground "#777777" :background "#222222" :box nil))))
   '(mode-line-inactive ((t (:foreground "#555555" :background "#222222" :box nil)))))

;; Can set them individually like this:
;; (set-face-foreground 'modeline "#777777")

;; The edge of the screen
(set-face-background 'fringe "#222222") ;;#222222, #111111




;;(set-cursor-color "purple")

;;(set-face-background 'hl-line "#222222")
;;(add-hook 'clojure-mode-hook (lambda () (hl-line-mode 1)))

(add-hook 'window-setup-hook (lambda () (set-cursor-color "purple")))
;;(add-hook 'window-setup-hook (lambda () (set-background-color "#111111")))
(add-hook 'window-setup-hook (lambda () (set-foreground-color "#AAAAAA")))



;; Disable boldness in all fonts:
(mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal :underline nil))
  (face-list))


;;; Fonts

;; You can determine the full name for a font in a window (under the cursor) by doing C-u C-x =

;; From: http://community.schemewiki.org/cgi-bin/scheme.cgi?Emacs
;;
;;(and window-system
;; (create-fontset-from-fontset-spec
;; (concat
;;       "-apple-monaco-medium-r-normal--12-*-*-*-*-*-fontset-monaco,"
;;       "ascii:-apple-monaco-medium-r-normal--12-100-*-*-m-100-mac-roman"))
;;     )

;;






;; To change a font size globally:
;; (set-face-attribute 'default nil :height 360)
;; (set-face-attribute 'default nil :height 240)
;; (set-face-attribute 'default nil :height 160)
;; (set-face-attribute 'default nil :height 120)


;; see http://www.gnu.org/software/emacs/manual/html_node/elisp/Attribute-Functions.html
;; for information about font faces
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "cyan" :weight normal)
              ("NEXT" :weight normal)
              ("DONE" :foreground "forest green" :weight normal)
              ("WAITING" :weight normal)
              ("HOLD" :weight normal)
              ("CANCELLED" :weight normal)
              ("PHONE" :weight normal))))

