# Guide to getting IRC up and running with emacs 24 and rcirc

- `rcirc` (http://emacswiki.org/emacs/rcirc) is the default irc package and will be started by doing `M-x irc` in your emacs
- It uses the `freenode` server by default (http://freenode.net)

## Registering with freenode

First of all, your going to want to register yourself a nickname with freenode..

When you start up irc you will get a prompt at which you can type commands to the server.

`/msg NickServ REGISTER password youremail@example.com` 

(From http://freenode.net/faq.shtml#nicksetup)

It will then send you an email with instructions on how to confirm your registration...

`/msg NickServ VERIFY REGISTER nick token`

Next time you log in you will need to identify yourself (see below)

`/msg NickServ IDENTIFY account password`

rcirc Kept my username somewhere, I could'nt find out where.

## Automatically Authenticating:

You can do this...
```lisp
(setq rcirc-authinfo '(("freenode" nickserv "username")))
```

But this would store your credentials openly in your emacs config.

So, what you want to do is add the following to your emacs config  ...
[link] https://github.com/jimbarritt/dot-files/blob/master/.emacs.d/jmdb/rcirc.md

```lisp
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
```

This magic will then look for either a file called `~/.authinfo` or `~/.authinfo.gpg`. The latter is an encrypted version, but to get started and test it out you can just create the first one. The format is:

```
host freenode port irc-nickserv login nick username "My FullName" password "password"
```
Beware this ref [2] because is has the wrong format. I found the correct one [here][3].

So now hopefully when you fire up irc (`M-x irc`) you are automatically connected and verified. Note that it will tell you first that you need to identify yourself but then it will show a subsequent message saying you are identified.


## References:

[1] http://www.emacswiki.org/emacs/rcircAutoAuthentication "rcirc Auth"
[2] http://superuser.com/questions/429937/how-do-you-hide-login-information-esp-passwords-in-emacs-init-file 
[3] http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-04/msg00096.html
