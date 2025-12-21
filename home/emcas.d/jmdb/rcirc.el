;; See rcirc.md in this folder for help!
(setq rcirc-server-alist
      '(("irc.freenode.net" :channels ("#emacs" "#clojure"))))



(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
(add-hook 'rcirc-mode-hook 'rcirc-omit-mode)

;; You could do this...
;;(setq rcirc-authinfo '(("freenode" nickserv "jmdb" "J1mbob")))
;; Here we load it up from the ~/.authinfo file instead...

;; First make sure we ignore any ~/.netrc files...
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))

;; Then extract what we want and set it into 'rcirc-authinfo
(defadvice rcirc (around rcirc=read-from-authinfo activate)
  "Allow rcirc to read authinfo from ~/.authinfo.gpg or ~/.authinfo (un-encrypted) via the auth-source API."
  (let ((p (auth-source-search :port '("irc-nickserv"))))   
    (let ((host (plist-get (first p) :host))
          (user (plist-get (third p) :user))
          (secret (plist-get (fourth p) :secret)))
      (setq rcirc-authinfo
            (list (list host 'nickserv user
                        (if (functionp secret)
                            (funcall secret)
                          secret))))))
  ad-do-it)


;;(describe-variable 'rcirc-authinfo)
;;(setq rcirc-authinfo nil)
;; (describe-function 'plist-get)
;; (fourth '(1 2 3 4))
;; (defadvice rcirc (around rcirc-read-from-authinfo activate)
;;   "Allow rcirc to read authinfo from ~/.authinfo.gpg or ~/.authinfo (un-encrypted) via the auth-source API."
;;   (let ((rcirc-authinfo rcirc-authinfo)
;;         (credentials (auth-source-search :port '("irc-nickserv"))))
;;     (dolist (p credentials)
;;       (let ((host (plist-get p :host))
;;             (user (plist-get p :user))
;;             (secret (plist-get p :secret)))
;;         (add-to-list 'rcirc-authinfo
;;                      (list host 'nickserv user
;;                            (if (functionp secret)
;;                                (funcall secret)
;;                              secret)))))
;;     ad-do-it))

;; :require '(:user :secret)

;; (defvar cred (auth-source-search :port '("irc-nickserv")))
;; cred
;; (first cred)
;;(plist-get (first cred) :host)



;; (dolist (p cred)
;;   (let ((host (plist-get p :host)))
;;     (print host)))


;; (auth-source-search :port '("irc-nickserv"))

;; (describe-function 'dolist)
;; (describe-function 'auth-source-search)

;; (let ((host )))

;; (let ((credentials ))  
;;   (message "Hello" (first  credentials)))

;; (list 1 3 4)

;; (describe-variable 'rcirc-authinfo)

;; (message "Jim " "foo")

;; (first '(1 2 3))
