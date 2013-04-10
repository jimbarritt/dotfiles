;;; Setup of color theme to be dark

(when (not (package-installed-p 'ir-black-theme))
  (package-install 'ir-black-theme))

(require 'ir-black-theme)


(set-default-font "-apple-Inconsolata-medium-normal-normal--20-180-75-75-m-0-iso10646-1")

;; Jims' Customisations:

;; Font locks
(global-font-lock-mode 1) ;; allows syntax highlighting to work
(custom-set-faces
 '(font-lock-doc-face ((t (:foreground "#669966" :bold nil))) 'now)
 '(font-lock-doc-string-face ((t (:foreground "#669966" :bold nil))) 'now)
 '(font-lock-string-face ((t (:foreground "#669966" :bold nil))) 'now)
 '(font-lock-builtin-face ((t (:foreground "#6699CC" :bold nil))) 'now)
 '(font-lock-keyword-face ((t (:foreground "#6699CC" :bold nil))) 'now)
 '(font-lock-function-name-face ((t (:foreground "#CCCC99" :bold nil))) 'now)
 '(font-lock-variable-name-face ((t (:foreground "#FFFFFF" :bold nil))) 'now)
 '(font-lock-preprocessor-face ((t (:foreground "#9999CC" :bold nil))) 'now)
 '(font-lock-type-face ((t (:foreground "#FFFFFF" :bold nil))) 'now))

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
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#FFFFFF"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#FFFFFF"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#FFFFFF"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#FFFFFF"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#FFFFFF"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#FFFFFF"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#FFFFFF"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#FFFFFF"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#FFFFFF"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#FF0000")))))

;; Highlight parens
(custom-set-faces
 '(show-paren-match-face ((t (:foreground "#FF00FF" :background "#000000" :bold t)))))

;; Shell
(custom-set-faces
 '(comint-highlight-input ((t (:foreground "#FFFFFF" :bold nil))))
 '(comint-highlight-prompt ((t (:foreground "#FFFFFF" :bold nil)))))
 
(custom-set-faces
   '(hl-line ((t (:background "#111111" :bold nil :underline nil :box nil)))))

(custom-set-faces
 '(mode-line ((t (:foreground "#AAAAAA" :background "#222222" :box nil))))
 '(header-line ((t (:foreground "#AAAAAA" :background "#222222" :box nil))))
 '(mode-line-inactive ((t (:foreground "#555555" :background "#222222" :box nil)))))

(custom-set-faces
   '(markdown-header-face ((t (:foreground "#999966" :bold nil)))))

(custom-set-faces
   '(magit-branch ((t (:foreground "#00CC66" :bold nil))))
   '(magit-diff-hunk-header ((t (:foreground "#AAAAAA" :background "#444444" :bold nil))))
   '(magit-diff-file-header ((t (:foreground "#888888" :background "#444444" :bold nil))))
   '(magit-diff-del ((t (:foreground "#CC3333"  :bold nil))))
   '(magit-header ((t (:foreground "#999966" :bold nil)))))

;; Can set them individually like this:

;; The edge of the screen
(set-face-background 'fringe "#222222") ;;#222222, #111111


;; IDO mode:
(custom-set-faces
 '(ido-first-match ((t (:foreground "#6666EE" :bold nil))))
 '(ido-subdir ((t (:foreground "#6622CC" :bold nil)))))


(set-foreground-color "#FFFFFF")
(add-hook 'window-setup-hook (lambda () (set-cursor-color "purple")))
(add-hook 'window-setup-hook (lambda () (set-foreground-color "#FFFFFF")))


;; Disable boldness in all fonts:
(mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal :underline nil :bold nil))
  (face-list))


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

