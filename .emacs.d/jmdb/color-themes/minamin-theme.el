;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html
;; Nicked mostly from https://raw.github.com/bbatsov/zenburn-emacs/master/zenburn-theme.el

(deftheme minamin "Minimal theme with light background")

(makunbound 'minamin-colors-alist)
(defvar minamin-colors-alist
  '(("minamin-fg"         . "#333c4e")
    ("minamin-bg-light"   . "#fffff5")
    ("minamin-bg-light-1" . "#ecece0")
    ("minamin-purple"     . "#6622CC") 
    ("minamin-grey-2"     . "#2B2B2B")
    ("minamin-grey-1"     . "#383838")
    ("minamin-grey"       . "#3F3F3F")
    ("minamin-grey+1"     . "#4F4F4F")
    ("minamin-grey+2"     . "#5F5F5F")
    ("minamin-grey+3"     . "#6F6F6F")
    ("minamin-grey+4"     . "#cccccc")
    ("minamin-red+1"      . "#DCA3A3")
    ("minamin-red"        . "#cc0000")
    ("minamin-red-1"      . "#BC8383")
    ("minamin-red-2"      . "#AC7373")
    ("minamin-red-3"      . "#9C6363")
    ("minamin-red-4"      . "#8C5353")
    ("minamin-orange"     . "#DFAF8F")
    ("minamin-yellow"     . "#F0DFAF")
    ("minamin-yellow-1"   . "#E0CF9F")
    ("minamin-yellow-2"   . "#D0BF8F")
    ("minamin-green-1"    . "#5F7F5F")
    ("minamin-green"      . "#7F9F7F")
    ("minamin-green+1"    . "#8FB28F")
    ("minamin-green+2"    . "#00a300")
    ("minamin-green+3"    . "#AFD8AF")
    ("minamin-green+4"    . "#BFEBBF")
    ("minamin-cyan"       . "#0c00cc")
    ("minamin-blue+1"     . "#1a87ff")
    ("minamin-blue"       . "#574dff")
    ("minamin-blue-1"     . "#271aff")
    ("minamin-blue-2"     . "#0e00e5")
    ("minamin-blue-3"     . "#0c00cc")
    ("minamin-blue-4"     . "#0b00b2")
    ("minamin-blue-5"     . "#08007f")
    ("minamin-magenta"    . "#6622CC"))
  "List of minamin colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro minamin-with-color-variables (&rest body)
  "`let' bind all colors defined in `minamin-colors-alist'.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   minamin-colors-alist))
     ,@body))

(defmacro theme-faces (theme-name)
  `(custom-theme-set-faces
    '@theme-name
    ))

(minamin-with-color-variables
 (custom-theme-set-faces
  'minamin
  `(default      ((t (:foreground ,minamin-fg :background , minamin-bg-light))))
  `(cursor       ((t (:foreground ,minamin-purple :background,  minamin-bg-light))))
  `(cursor-color ((t (:foreground ,minamin-purple :background,  minamin-bg-light))))
  `(fringe       ((t (:foreground ,minamin-bg-light :background ,minamin-grey+4))))

  `(header-line  ((t (:foreground ,minamin-bg-light
                                 :background ,minamin-grey+3
                                 :box (:color, minamin-grey+3 :line-width 5 :style nil)))))

  `(mode-line    ((t (:foreground ,minamin-grey+4 
                                  :background ,minamin-grey+3 
                                  :box (:color ,minamin-grey+3 :line-width 2 :style nil)))))

  `(mode-line-buffer-id ((t (:foreground ,minamin-grey+4 :background nil))))

  `(mode-line-inactive  ((t (:foreground ,minamin-bg-light 
                                         :background ,minamin-bg-light 
                                         :box (:color ,minamin-grey+4 :line-width 2 :style nil)))))


  `(highlight   ((t (:background ,minamin-bg-light-1 :inverse t))))
  `(region      ((t (:background ,minamin-yellow))))

   ;;;; font lock
   `(font-lock-builtin-face           ((t (:foreground ,minamin-blue-4))))
   `(font-lock-comment-face           ((t (:foreground ,minamin-grey+3))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,minamin-grey+3))))
   `(font-lock-constant-face          ((t (:foreground ,minamin-blue-4))))
   `(font-lock-doc-face               ((t (:foreground ,minamin-green))))
   `(font-lock-doc-string-face        ((t (:foreground ,minamin-green))))
   `(font-lock-function-name-face     ((t (:foreground ,minamin-grey+1))))
   `(font-lock-keyword-face           ((t (:foreground ,minamin-blue-4))))
   `(font-lock-negation-char-face     ((t (:foreground ,minamin-fg))))
   `(font-lock-preprocessor-face      ((t (:foreground ,minamin-grey+3))))
   `(font-lock-string-face            ((t (:foreground ,minamin-green+2))))
   `(font-lock-type-face              ((t (:foreground ,minamin-fg))))
   `(font-lock-variable-name-face     ((t (:foreground ,minamin-fg))))
   `(font-lock-warning-face           ((t (:foreground ,minamin-fg))))

   `(magit-branch                     ((t (:foreground ,minamin-fg :background,minamin-bg-light :box nil))))
   `(magit-section-title              ((t (:foreground ,minamin-fg :background,minamin-bg-light :box nil))))

   '(rainbow-delimiters-depth-1-face   ((t (:foreground "#9933FF"))))
   '(rainbow-delimiters-depth-2-face   ((t (:foreground "#9999CC"))))
   '(rainbow-delimiters-depth-3-face   ((t (:foreground "#554499"))))
   '(rainbow-delimiters-depth-4-face   ((t (:foreground "#669999"))))
   '(rainbow-delimiters-depth-5-face   ((t (:foreground "#336666"))))
   '(rainbow-delimiters-depth-6-face   ((t (:foreground "#442266"))))
   '(rainbow-delimiters-depth-7-face   ((t (:foreground "#9999CC"))))
   '(rainbow-delimiters-depth-8-face   ((t (:foreground "#666699"))))
   '(rainbow-delimiters-depth-9-face   ((t (:foreground "#99CCCC"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "#FF0000"))))

   '(show-paren-match-face ((t (:foreground "#ff33cc" :background nil :bold t))))

   `(comint-highlight-prompt           ((t (:foreground ,minamin-fg))))
   `(comint-highlight-input            ((t (:foreground ,minamin-grey+3))))


  ))

;; The shell...

(minamin-with-color-variables
  (custom-theme-set-variables
   'minamin
;;;;; ansi-color
;;http://stackoverflow.com/questions/6549622/adjusting-shell-mode-color-schemes
   ;; (setq ansi-color-names-vector
   ;;       ["black" "red" "green" "yellow" "PaleBlue" "magenta" "cyan" "white"])

   `(ansi-color-names-vector [,minamin-bg-light ,minamin-red , minamin-green-1 ,"yellow" 
                              ,"blue" ,minamin-magenta ,minamin-cyan ,minamin-fg])

;;;;; fill-column-indicator
   `(fci-rule-color ,minamin-bg-light-1)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,minamin-red-1)
       ( 40. . ,minamin-red)
       ( 60. . ,minamin-orange)
       ( 80. . ,minamin-yellow-2)
       (100. . ,minamin-yellow-1)
       (120. . ,minamin-yellow)
       (140. . ,minamin-green-1)
       (160. . ,minamin-green)
       (180. . ,minamin-green+1)
       (200. . ,minamin-green+2)
       (220. . ,minamin-green+3)
       (240. . ,minamin-green+4)
       (260. . ,minamin-cyan)
       (280. . ,minamin-blue-2)
       (300. . ,minamin-blue-1)
       (320. . ,minamin-blue)
       (340. . ,minamin-blue+1)
       (360. . ,minamin-magenta)))
   `(vc-annotate-very-old-color ,minamin-magenta)
   `(vc-annotate-background ,minamin-bg-light)
   ))







(provide-theme 'minamin)

;;; Rainbow Support - Adding font lock so you can see all the colors defined.

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar minamin-add-font-lock-keywords nil
  "Whether to add font-lock keywords for zenburn color names.
In buffers visiting library `zenburn-theme.el' the zenburn
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar minamin-colors-font-lock-keywords nil)

(defadvice rainbow-turn-on (after minamin activate)
  "Maybe also add font-lock keywords for minamin colors."
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (or minamin-add-font-lock-keywords
                 (equal (file-name-nondirectory (buffer-file-name))
                        "minamin-theme.el")))
    (unless minamin-colors-font-lock-keywords
      (setq minamin-colors-font-lock-keywords
            `((,(regexp-opt (mapcar 'car minamin-colors-alist) 'words)
               (0 (rainbow-colorize-by-assoc minamin-colors-alist))))))
    (font-lock-add-keywords nil minamin-colors-font-lock-keywords)))

(defadvice rainbow-turn-off (after minamin activate)
  "Also remove font-lock keywords for minamin colors."
  (font-lock-remove-keywords nil minamin-colors-font-lock-keywords))

(set-cursor-color 'purple)
