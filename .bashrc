#bash configuration

# Bash history - ignore duplicates and have a big history
export HISTSIZE=10000
shopt -s histappend
export HISTIGNORE='&:ls:cd ~:cd ..:[bf]g:exit:h:history'
export HISTCONTROL=erasedups
export PROMPT_COMMAND='history -a'



# Output everything to be in emacs format
set -o emacs

# In emacs, this gets set to 'dumb' by default so here we override it so that ls prints out colors correctly
#export TERM=xterm
export TERM=xterm-256color # And this should make it look nicer

export PATH="."
export PATH="$PATH:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/texbin:/usr/X11/bin"
export PATH="$PATH:/usr/local/share/python"
export PATH="$PATH:/Applications"
export PATH="$PATH:/usr/texbin"
export PATH="~/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/Cellar/rabbitmq/3.0.4/sbin:$PATH"

export SVN_EDITOR=emacs

source ~/bin/git_completion.sh

# \u -- username
#\W pwd
# PS1='\[\e[1;32m\][\u@\h \W]\$\[\e[0m\] '

#http://www.isthe.com/chongo/tech/comp/ansi_escapes.html
#http://en.wikipedia.org/wiki/ANSI_escape_code#Colors
START_GREEN_BOLD="\[\e[1;32m\]"
START_GREEN_NORMAL="\[\e[0;32m\]"
END_GREEN="\[\e[0m\]"

# Configure the command prompt
# it is <PROMPT_COMMAND><PS1>
export PROMPT_COMMAND=''

# See - http://www.cyberciti.biz/tips/howto-linux-unix-bash-shell-setup-prompt.html
# Also http://hints.macworld.com/article.php?story=20001224021638403
# Edit /etc/hostconfig to force it away from K2.local
#$(hostname)
export CURRENT_HOST=K2
hostname=K2
# \u - username
# \h - hostname
# \W - current dir
# \w - full working dir
NORMAL_FONT=`tput sgr0`
export PS1='\u@$CURRENT_HOST:\w\[\e[0;32m\]$(__git_ps1 " [%s]")\[\e[0m\]\n\$ '



alias ll='ls -larthG'
alias ld='ls -d */'
alias du='du -h'
alias ls='ls -G' #To get colorised listing
alias emacs='emacsclient -n'
alias pywork='source ~/bin/pywork'

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

[[ -s "$HOME/.bashrc_workstation" ]] && source "$HOME/.bashrc_workstation"

function tabname {
  printf "\e]1;$1\a"
}
 
function winname {
  printf "\e]2;$1\a"
}
