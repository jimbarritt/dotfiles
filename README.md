# Jims dot files

## Important Mac Setup

In order to have a sublime typing experience particularly in vim or emacs, you need to adjust the keyboard. It is a simple hack but it really makes a difference.

You can change the return key so that when its pressed with other keys it acts like a control key.

Then you can also change the CAPS LOCK key to do the same thing. This means they continue to function normally except when pressed in conjunction with other keys which makes your fingers and typing so much better particularly in emacs.

YOu can do this using the wonderful utility [karabiner](https://pqrs.org/osx/karabiner/) note that these modifications are called "complex modifications" and have to be imported from their website - all the buttons for this are on the UI.

## Emacs

THe next most important application is of course, emacs. 

I have many customisations in here but the most important is to map the right command key to be the meta key 

## Bash

The `.bashrc` file is where it all kicks off. This has a set of generic customisations for bash which should work on any machine. If there are customisations that are specific to a machine (i.e. they need a hard coded path, then this file sources the file `.bashrc_workstation`, so they can be stored there.

## Git

I have some aliases set up for git in `.gitconfig` particularly interesting is the "serve" option - see http://coderwall.com/p/eybtga. This means I just need to type:

    git serve

In a git rep dir and then people can do:

    git clone git://your.laptop.ip.or.hostname/


