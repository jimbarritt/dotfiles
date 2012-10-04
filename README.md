# Jims dot files

Here is where all my 'nix knowledge lies.

The `.bashrc` file is where it all kicks off. This has a set of generic customisations for bash which should work on any machine. If there are customisations that are specific to a machine (i.e. they need a hard coded path, then this file sources the file `.bashrc_workstation`, so they can be stored there.

# Git

I have some aliases set up for git in `.gitconfig` particularly interesting is the "serve" option - see http://coderwall.com/p/eybtga. This means I just need to type:

    git serve

In a git rep dir and then people can do:

    git clone git://your.laptop.ip.or.hostname/


