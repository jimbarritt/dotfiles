;; See rcirc.md in this folder for help!
(setq rcirc-server-alist
      '(("irc.freenode.net" :channels ("#emacs" "#clojure"))))

(defadvice rcirc (around rcirc-read-from-authinfo activate)
  "Allow rcirc to read authinfo from ~/.authinfo.gpg or ~/.authinfo (un-encrypted) via the auth-source API."
  (let ((rcirc-authinfo rcirc-authinfo)
        (credentials (auth-source-search :port '("irc-nickserv")
                                         :require '(:user :secret))))
    (dolist (p credentials)
      (let ((host (plist-get p :host))
            (user (plist-get p :user))
            (secret (plist-get p :secret)))
        (add-to-list 'rcirc-authinfo
                     (list host 'nickserv user
                           (if (functionp secret)
                               (funcall secret)
                             secret)))))
    ad-do-it))

