;; Magit needs to be installed first
;; TODO - work out how to only do this if magit is there...

(require 'magit)


(when (not (package-installed-p 'nrepl))
  (package-install 'nrepl))

(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*nrepl*")


(server-start) ;; Start up the emacs server so that you can call emacsclient from the command line


(require 'lambda-mode)

;; Setup environment variables from the shell...
(defun env-var-from-shell (varname)
  (replace-regexp-in-string
   "[[:space:]\n]*$" ""
   (shell-command-to-string (concat "$SHELL -l -c 'echo $" varname
"'"))))


(defun setenv-from-shell (varname)
  (setenv varname (env-var-from-shell varname)))
