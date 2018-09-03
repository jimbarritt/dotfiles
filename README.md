# Jims dot files

## Important Mac Keyboard Setup

In order to have a sublime typing experience particularly in vim or emacs, you need to adjust the keyboard. It is a simple hack but it really makes a difference.

You can change the return key so that when its pressed with other keys it acts like a control key.

Then you can also change the CAPS LOCK key to do the same thing. This means they continue to function normally except when pressed in conjunction with other keys which makes your fingers and typing so much better particularly in emacs.

YOu can do this using the wonderful utility [karabiner](https://pqrs.org/osx/karabiner/) note that these modifications are called "complex modifications" and have to be imported from their website - all the buttons for this are on the UI.]

Remap option key to "META" in the terminal app so you cvan use emacs

Also what is quite annoying if you have a UK keyboard is that the # symbol is on "OPTION+3" because the £ symbol is default.

So you can switch the input source to ENGLISH US which means that it will now put th # on SHIFT+3 instead. the only downside is you cant now to OPT+2 for the euro symbol. SO this needs to be mapped back in emacs. You can get it if you really need it from the symbol picker.

OR you can create a custom input source. Use this program https://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=ukelele

This allows you to create a layout from an existing one (British) and then just swap the £ for the #

Dont Follow these instructions https://superuser.com/questions/665494/how-to-make-a-custom-keyboard-layout-in-os-x#665726

There is now an "install for current user" option in ukelele - You need to quit and restart the system preferences and it shows up.

Now I can type € 

## Color themes
 
The NORD theme is very good https://raw.githubusercontent.com/arcticicestudio/nord-terminal-app/develop/src/xml/Nord.terminal

also DOOM for emacs https://github.com/hlissner/emacs-doom-themes/blob/screenshots/README.md#doom-one-light-wip

based on NORD

## Homebrew

Vital for anything really

https://brew.sh/

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


