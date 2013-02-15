#bash configuration

# Bash history - ignore duplicates and have a big history
export HISTCONTROL=ignoredups
export HISTSIZE=10000

# Output everything to be in emacs format
set -o emacs

# In emacs, this gets set to 'dumb' by default so here we override it so that ls prints out colors correctly
export TERM=xterm

export PATH="."
export PATH="$PATH:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/texbin:/usr/X11/bin"
export PATH="$PATH:/usr/local/share/python"
export PATH="$PATH:/Applications"
export PATH="$PATH:/usr/texbin"
export PATH="~/bin:$PATH"

export SVN_EDITOR=emacs

source ~/bin/git_completion.sh

# \u -- username
#\W pwd
# PS1='\[\e[1;32m\][\u@\h \W]\$\[\e[0m\] '

#http://www.isthe.com/chongo/tech/comp/ansi_escapes.html
START_GREEN_BOLD="\[\e[1;32m\]"
START_GREEN_NORMAL="\[\e[0;32m\]"
END_GREEN="\[\e[0m\]"

# Configure the command prompt
# it is <PROMPT_COMMAND><PS1>
export PROMPT_COMMAND=''

# See - http://www.cyberciti.biz/tips/howto-linux-unix-bash-shell-setup-prompt.html
#$(hostname)
export CURRENT_HOST=K2
hostname=K2
# \u - username
# \h - hostname
# \W - current dir
# \w - full working dir
NORMAL_FONT=`tput sgr0`
export PS1='\u@$CURRENT_HOST:\w\[\e[0;32m\]$(__git_ps1 " [%s]")\[\e[0m\]\n\$ '



alias ll='ls -lartG'
alias ls='ls -G' #To get colorised listing
alias emacs='emacsclient -n'

declare -x CLICOLOR=1
declare -x LSCOLORS=gxfxcxdxbxegedabagacad

export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home/

# For RVM (Ruby environment manager)
# This basically saying "if the symbolic link to rvm exists, execute it"
# this sets rvm up as a shell function which seems to be nescessary for it to work.
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"


PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"


