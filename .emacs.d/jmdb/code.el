(when (not (package-installed-p 'nrepl))
  (package-install 'nrepl))

(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*nrepl*")

(when (not (package-installed-p 'paredit))
  (package-install 'paredit))

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

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


