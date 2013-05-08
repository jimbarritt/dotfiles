;;; Setup of color theme to be light


(global-font-lock-mode 1) ;; allows syntax highlighting to work

;; To change the font size interactively per buffer:
;; Up:    C-x C-+, C-x C-=
;; Down:  C-x C--
;; Reset: C-x C-0

;;(set-default-font "-apple-Inconsolata-medium-normal-normal--14-180-75-75-m-0-iso10646-1")
(set-default-font "-apple-consolas-medium-normal-normal--14-180-75-75-m-0-iso10646-1")

;; Color themes
(add-to-list 'load-path "~/.emacs.d/lib/color-theme")
(require 'color-theme)
(color-theme-initialize)

;;(color-theme-greiner)
;;(color-theme-snowish)
;;(color-theme-gtk-ide)
(color-theme-standard)




;; Jims' Customisations:


(custom-set-faces
 '(font-lock-doc-face ((t (:foreground "#669966" :bold nil))) 'now)
 '(font-lock-doc-string-face ((t (:foreground "#669966" :bold nil))) 'now)
 '(font-lock-string-face ((t (:foreground "#669966" :bold nil))) 'now)
 '(font-lock-builtin-face ((t (:foreground "#000080" :bold nil))) 'now)
 '(font-lock-keyword-face ((t (:foreground "#000080" :bold nil))) 'now)
 '(font-lock-function-name-face ((t (:foreground "#333333" :bold nil))) 'now)
 '(font-lock-variable-name-face ((t (:foreground "#000080" :bold nil))) 'now)
 '(font-lock-preprocessor-face ((t (:foreground "#666666" :bold nil))) 'now)
 '(font-lock-type-face ((t (:foreground "#333333" :bold nil))) 'now)
 '(font-lock-comment-delimiter-face ((t (:foreground "#555555" :bold nil))) 'now)
 '(font-lock-comment-face ((t (:foreground "#888888" :bold nil))) 'now)
 '(font-lock-constant-face ((t (:foreground "#660099" :bold nil))) 'now)
 '(font-lock-negation-char-face ((t (:foreground "#FF1122" :bold nil))) 'now)
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#FF1122" :bold nil))) 'now)
 '(font-lock-regexp-grouping-construct ((t (:foreground "#FF1122" :bold nil))) 'now)
 '(font-lock-warning-face ((t (:foreground "#FF1122" :bold nil))) 'now)
)




;; Diff mode:
(custom-set-faces
 '(diff-added ((t (:foreground "Green"))) 'now)
 '(diff-removed ((t (:foreground "Red"))) 'now))

;; dif-hl:

(custom-set-faces
 '(diff-hl-insert ((t (:foreground "Green4" :background "#224422"))) 'now)
 '(diff-hl-delete ((t (:foreground "Red4" :background "#AA2222"))) 'now))

;; The modeline:

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#9933FF"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#9999CC"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#554499"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#669999"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#336666"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#CCCCFF"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#9999CC"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#666699"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#99CCCC"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#FF0000")))))

;; Highlight parens
(custom-set-faces
 '(show-paren-match-face ((t (:foreground "#FF00FF" :background "#222222" :bold t)))))

;; Shell
(custom-set-faces
 '(comint-highlight-input ((t (:foreground "#555555" :bold nil))))
 '(comint-highlight-prompt ((t (:foreground "#555555" :bold nil)))))
 

(global-hl-line-mode)

(custom-set-faces
   '(hl-line ((t (:background "#EEEEEE" :bold nil :underline nil :box nil)))))



(custom-set-faces
 '(mode-line ((t (:foreground "#444444" :background "#CCCCCC" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#444444" :background nil :box nil))))
 '(header-line ((t (:foreground "#777777" :background "#CCCCCC" :box nil))))
 '(header-line-inactive ((t (:foreground "#777777" :background "#AAAAAA" :box nil))))
 '(mode-line-inactive ((t (:foreground "#444444" :background "#DDDDDD" :box nil))))
 
)

(custom-set-faces
   '(markdown-header-face ((t (:foreground "#999966" :bold nil)))))

(custom-set-faces
   '(magit-branch ((t (:foreground "#00CC66" :bold nil))))
   '(magit-diff-hunk-header ((t (:foreground "#AAAAAA" :background "#444444" :bold nil))))
   '(magit-diff-file-header ((t (:foreground "#888888" :background "#444444" :bold nil))))
   '(magit-diff-del ((t (:foreground "#CC3333"  :bold nil))))
   '(magit-header ((t (:foreground "#999966" :bold nil)))))

;; Can set them individually like this:
;; (set-face-foreground 'modeline "#777777")

;; The edge of the screen
(set-face-background 'fringe "#EEEEEE") ;;#222222, #111111


;; IDO mode:
(custom-set-faces
 '(ido-first-match ((t (:foreground "#6666EE" :bold nil))))
 '(ido-subdir ((t (:foreground "#6622CC" :bold nil))))
 '(minibuffer-prompt ((t (:foreground "#666666" :bold nil)))))

minibuffer-prompt

(add-hook 'window-setup-hook (lambda () (set-cursor-color "purple")))
(add-hook 'window-setup-hook (lambda () (set-foreground-color "#333333")))
(add-hook 'window-setup-hook (lambda () (set-background-color "#FEFEFE")))

(set-cursor-color "purple")
(set-foreground-color "#333333")
(set-background-color "#FEFEFE")




;; Disable boldness in all fonts:
(mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal :underline nil :bold nil))
  (face-list))



;; see http://www.gnu.org/software/emacs/manual/html_node/elisp/Attribute-Functions.html
;; for information about font faces
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "blue" :weight normal)
              ("NEXT" :weight normal)
              ("DONE" :foreground "green" :weight normal)
              ("WAITING" :weight normal)
              ("HOLD" :weight normal)
              ("CANCELLED" :weight normal)
              ("PHONE" :weight normal))))

