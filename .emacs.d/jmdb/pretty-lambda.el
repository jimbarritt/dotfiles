(defun lisp-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun clojure-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(fn\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))


(add-hook 'interactive-lisp-mode-hook 'lisp-pretty-lambdas)
(add-hook 'clojure-mode-hook 'clojure-pretty-lambdas)
(add-hook 'emacs-lisp-mode-hook       'lisp-pretty-lambdas)
(add-hook 'lisp-mode-hook             'lisp-pretty-lambdas)
(add-hook 'lisp-interaction-mode-hook 'lisp-pretty-lambdas)

