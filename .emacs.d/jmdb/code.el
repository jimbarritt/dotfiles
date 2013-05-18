
(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))


(when (not (package-installed-p 'nrepl))
  (package-install 'nrepl))

(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*nrepl*")

;; https://github.com/syohex/emacs-git-gutter
;; (when (not (package-installed-p 'git-gutter)) 
;;   (package-install 'git-gutter))
;; This doesnt seem to work
;; (require 'git-gutter)
;; (global-git-gutter-mode t)

(when (not (package-installed-p 'paredit))
  (package-install 'paredit))

(when (not (package-installed-p 'clojure-mode))
  (package-install 'clojure-mode))

;; You need to do brew install w3m
(when (not (package-installed-p 'w3m))
  (package-install 'w3m))


(when (not (package-installed-p 'nav))
  (package-install 'nav))

(when (not (package-installed-p 'itail))
  (package-install 'itail))

(require 'itail)

(when (not (package-installed-p 'multi-web-mode))
  (package-install 'multi-web-mode))
(require 'multi-web-mode)

(when (not (package-installed-p 'haml-mode))
  (package-install 'haml-mode))
(require 'haml-mode)

(when (not (package-installed-p 'slim-mode))
  (package-install 'slim-mode))
(require 'slim-mode)





(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)




(require 'paredit)
(autoload 'paredit-mode "paredit"
      "Minor mode for pseudo-structurally editing Lisp code." t)

(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))

(when (not (package-installed-p 'sass-mode))
  (package-install 'sass-mode))

(require 'sass-mode)

(when (not (package-installed-p 'ecukes))
  (package-install 'ecukes))


(require 'magit) ;; Magit needs to be installed first - try to work out how to only load if present

;; (require 'lambda-mode)
;; (add-hook 'emacs-lisp-mode-hook       (lambda () (lambda-mode)))
;; (add-hook 'lisp-mode-hook             (lambda () (lambda-mode)))
;; (add-hook 'lisp-interaction-mode-hook (lambda () (lambda-mode)))
;; (add-hook 'clojure-mode-hook (lambda () (lambda-mode)))


(load "jmdb/pretty-lambda.el")
;; lambda mode seems to work well 
(load "jmdb/eshell.el")
(load "jmdb/pg.el")
;;(load "jmdb/diff-hl.el")
;;(require 'diff-hl)
;;(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
;;(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
(load "jmdb/shell.el")
(load "lib/tidy.el")

(add-hook 'comint-mode-hook 'my-comint-init) 

(require 'tidy)




(require 'pg)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq explicit-shell-file-name "/bin/bash")

(load "jmdb/nodejs-mode.el")
(require 'nodejs-mode)

;; Autocomplete dependencies...
(load "jmdb/popup-el/popup.el")
(require 'popup)

(load "jmdb/fuzzy-el/fuzzy.el")
(require 'fuzzy)

(load "jmdb/popup-el/popup.el")

(add-to-list 'load-path "~/.emacs.d/jmdb/auto-complete")
(load "jmdb/auto-complete/auto-complete.el")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/jmdb/auto-complete/dict")

(require 'auto-complete-config)
(ac-config-default)

(setq ac-comphist-file (concat "~/tmp/" "ac-comphist.dat"))

(global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
(setq ac-disable-inline t)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-start 2)
(setq ac-candidate-menu-min 0)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic
               ac-source-yasnippet))

(when (not (package-installed-p 'ac-nrepl))
  (package-install 'ac-nrepl))

(require 'ac-nrepl)

(when (not (package-installed-p 'slime))
  (package-install 'slime))

(when (not (package-installed-p 'ac-slime))
  (package-install 'ac-slime))

(require 'ac-slime)

(when (not (package-installed-p 'pos-tip))
  (package-install 'pos-tip))

;;(require 'pos-tip)

(add-hook 'slime-mode-hook 'set-up-slime-ac)


(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode nrepl-mode
                lisp-mode textile-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))

(ac-flyspell-workaround)
;; auto-complete
;; Examples
(set-face-background 'ac-candidate-face "#555555")
(set-face-underline 'ac-candidate-face "#555555")
(set-face-background 'ac-selection-face "#777777")

;;;;Key triggers
;; (define-key ac-completing-map (kbd "C-M-n") 'ac-next)
;; (define-key ac-completing-map (kbd "C-M-p") 'ac-previous)
;; (define-key ac-completing-map "\t" 'ac-complete)
;; (define-key ac-completing-map (kbd "M-RET") 'ac-help)
;; (define-key ac-completing-map "\r" 'nil)



(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)

(setq nrepl-hide-special-buffers t)

(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)


;;nREPL

(add-hook 'nrepl-interaction-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)))

(add-hook 'nrepl-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)
            (define-key nrepl-mode-map
              (kbd "{") 'paredit-open-curly)
            (define-key nrepl-mode-map
              (kbd "}") 'paredit-close-curly)))

(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'nrepl-mode-hook 'subword-mode)

;;Auto Complete
;;(live-add-pack-lib "ac-nrepl")
(require 'ac-nrepl )
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;;; Monkey Patch nREPL with better behaviour:
;; Well it works if you have the other functions
;; (defun nrepl-region-for-expression-at-point ()
;;   "Return the start and end position of defun at point."
;;   (when (and (live-paredit-top-level-p)
;;              (save-excursion
;;                (ignore-errors (forward-char))
;;                (live-paredit-top-level-p)))
;;     (error "Not in a form"))

;;   (save-excursion
;;     (save-match-data
;;       (ignore-errors (live-paredit-forward-down))
;;       (paredit-forward-up)
;;       (while (ignore-errors (paredit-forward-up) t))
;;       (let ((end (point)))
;;         (backward-sexp)
;;         (list (point) end)))))

(setq nrepl-port "6678")
