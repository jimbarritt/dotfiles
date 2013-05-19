;;; Settings for the mode line

;; Prevent it having a 3D box
(set-face-attribute 'mode-line nil :box nil)


;; Display the time in the mode bar.
;; http://www.emacswiki.org/emacs/DisplayTime
;; Use this to play with the format string...
;; (format-time-string "%R %Y-%m-%d [%Z]" (current-time))

;; Display the time in the mode line:
(setq display-time-format "%R %Y-%m-%d [%Z]")
(setq display-time-string-forms
     '((format-time-string display-time-format (current-time))))

(setq display-time-and-date t)
(display-time-mode 1)

(display-battery-mode)

;; Making all the modes show up as symbols
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas-minor-mode . " γ")
    (paredit-mode . " Φ")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . " τ")
    (volatile-highlights-mode . " υ")
    (elisp-slime-nav-mode . " δ")
    (nrepl-mode . " ηζ")
    (nrepl-interaction-mode . " ηζ")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (clojure-mode . "λ")
    (EL-mode . "λ")
    (lambda-mode . "")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "EL")
    (markdown-mode . "md"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")
 
 
(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))
 
 
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

 
 
;;; Greek letters - C-u C-\ greek ;; C-\ to revert to default
;;; ς ε ρ τ υ θ ι ο π α σ δ φ γ η ξ κ λ ζ χ ψ ω β ν μ




