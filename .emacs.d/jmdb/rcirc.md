# Guide to getting IRC up and running with emacs 24 and rcirc

- `rcirc` (http://emacswiki.org/emacs/rcirc) is the default irc package and will be started by doing `M-x irc` in your emacs
- It uses the `freenode` server by default (http://freenode.net)


## Registering with freenode

First of all, your going to want to register yourself a nickname with freenode..

When you start up irc you will get a prompt at which you can type commands to the server.

```
/msg NickServ REGISTER password youremail@example.com
```

(From http://freenode.net/faq.shtml#nicksetup)

It will then send you an email with instructions on how to confirm your registration...

```
/msg NickServ VERIFY REGISTER nick token
```

By default, rcirc uses the value of `(user-login-name)` which should be your user id from your mac / pc (http://www.gnu.org/software/emacs/manual/html_node/rcirc/Configuration.html)

Next time you log in you will need to identify yourself (see below)

```
/msg NickServ IDENTIFY account password
```

rcirc Kept my username somewhere, I could'nt find out where.

## Automatically Authenticating:

You can do this...
```lisp
(setq rcirc-authinfo '(("freenode" nickserv "username" password "password")))
```

But this would store your credentials openly in your emacs config.

Originally, I found a reference which involved "advising" the rcirc function (http://www.gnu.org/software/emacs/manual/html_node/elisp/Around_002dAdvice.html#Around_002dAdvice) [[1] [1]] ...

However, this didn't seem to work for me.

There are a couple of things I learned. 

Firstly there is a function called `auth-source-search` which you can use to load up credentials from files in your home dir. 

It looks for files with the `netrc` format for storing credentials. It looks in a set of files defined by the variable `auth-sources`.

This by default is set to `("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")` (You can see this by doing `(describe-variable 'auth-sources)`

Now, there were two problems with the source I found. Firstly, if you alread have a `~/.netrc` file, it won't look in the other files. 

We could add our entry to `~/.netrc` but this file could be automatially created by other programs. So lets change our `auth-sources` to only look for `authinfo` or `authinfo.gpg` (we'll come to the `gpg` part later.

```lisp
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))
```

The code I came up with was this...

```lisp
(let ((p (auth-source-search :port '("irc-nickserv"))))   
  (let ((host (plist-get (first p) :host))
        (user (plist-get (third p) :user))
        (secret (plist-get (fourth p) :secret)))
    (setq rcirc-authinfo
          (list (list host 'nickserv user
                      (if (functionp secret)
                          (funcall secret)
                        secret))))))
```

We can now create `~/.authinfo` in it we put:

```
machine freenode 
  port irc-nickserv 
  login NICK
  password PASSWORD
```

So now hopefully when you fire up irc (`M-x irc`) you are automatically connected and verified. Note that it will tell you first that you need to identify yourself but then it will show a subsequent message saying you are identified.

## Encrypt your authinfo

The next step is to not have our password out in the open.

Download the DMG file from [[4] [4]]

I found some instructions [[5] [5]] but they were a bit out of date concerning the verification of the downloaded files' checksum. It is now published with a sha1 checksum so to verify [[6] [6]]...

```
openssl sha1 GPGTools-20120318.dmg | grep 184bf74e55c509da0aa4943ab7cc39ecd5caf99f 
```

Then it's a simple matter of encrypting your `~/.authinfo` file

```
gpg -c .authinfo
```

You can tell this is working because you see in the Messages window you will see

```
Decrypting /Users/<username>/.authinfo.gpg...done
```

## Setting default channels

We can also set ourselves up so we join some channels automatically...
```lisp
(setq rcirc-server-alist
      '(("irc.freenode.net" :channels ("#emacs" "#clojure"))))
```

## References

[1]: http://www.emacswiki.org/emacs/rcircAutoAuthentication "rcirc authentication"
[2]: http://superuser.com/questions/429937/how-do-you-hide-login-information-esp-passwords-in-emacs-init-file "Dodgy format of authinf file"
[3]: http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-04/msg00096.html "Good version of the authinf file"
[4]: https://www.gpgtools.org/installer/index.html#releasenotes "Pgp installer os x"
[5]: http://www.robertsosinski.com/2008/02/18/working-with-pgp-and-mac-os-x/ "Working with pgp on os x"
[6]: http://support.apple.com/kb/HT1652 "How to verify a sha1 digest"
[7]: http://www.cyberciti.biz/tips/linux-how-to-encrypt-and-decrypt-files-with-a-password.html


[[1][1]] http://www.emacswiki.org/emacs/rcircAutoAuthentication

[[2][2]] http://superuser.com/questions/429937/how-do-you-hide-login-information-esp-passwords-in-emacs-init-file 

[[3][3]] http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-04/msg00096.html 
