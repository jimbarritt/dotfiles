# Guide to getting IRC up and running with emacs 24 and rcirc (http://emacswiki.org/emacs/rcirc)

- `rcirc` is the default irc package and will be started by doing `M-x irc` in your emacs
- It uses the `freenode` server by default (http://freenode.net)

# Registering with freenode

First of all, your going to want to register yourself a nickname with freenode..

# Automatically Authenticating:

You can do this...
(setq rcirc-authinfo '(("freenode" nickserv "username" "password")))

But obviously

References:

This one is incorrect in the format of the .authinfo file 
http://superuser.com/questions/429937/how-do-you-hide-login-information-esp-passwords-in-emacs-init-file 

This one has the right settings
http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-04/msg00096.html
