#bash configuration

# Output everything to be in emacs format
set -o emacs

# In emacs, this gets set to 'dumb' by default so here we override it so that ls prints out colors correctly
export TERM=xterm

export PATH="."
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/texbin:/usr/X11/bin"
export PATH="$PATH:/usr/local/share/python"
export PATH="$PATH:/Applications"
export PATH="$PATH:/usr/texbin"

export JAVACC_HOME="/Users/jim/work/code/external/javacc-5.0"

export SVN_EDITOR=emacs
export JMETER_HOME=/Users/jim/work/code/external/apache/jakarta-jmeter-2.3.4


source ~/bin/git_completion.sh

# \u -- username
#\W pwd
# PS1='\[\e[1;32m\][\u@\h \W]\$\[\e[0m\] '
START_GREEN="\[\e[1;32m\]"
END_GREEN="\[\e[0m\]"

# Configure the command prompt
# it is <PROMPT_COMMAND><PS1>
export PROMPT_COMMAND=''

# See - http://www.cyberciti.biz/tips/howto-linux-unix-bash-shell-setup-prompt.html
#$(hostname)
export CURRENT_HOST=coco
# \u - username
# \h - hostname
# \W - current dir
# \w - full working dir
export PS1='\u@$CURRENT_HOST:\w\[\e[1;32m\]$(__git_ps1 " [%s]")\[\e[0m\]\n\$-> '



alias ll='ls -lartG'
alias ls='ls -G' #To get colorised listing

declare -x CLICOLOR=1
declare -x LSCOLORS=gxfxcxdxbxegedabagacad

# For RVM (Ruby environment manager)
# This basically saying "if the symbolic link to rvm exists, execute it"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# Amazon EC2 for sukrupa
export EC2_HOME=/Users/jim/sukrupa/EC2/ec2-api-tools-1.4.2.2 # move this to ~/.ec2
export PATH=$PATH:$EC2_HOME/bin
export PATH=~/bin:$PATH
export EC2_PRIVATE_KEY=`ls $EC2_HOME/pk-*.pem`
export EC2_CERT=`ls $EC2_HOME/cert-*.pem`
export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home/

#export PATH=~/work/code/external/scala-2.8.0.final/bin:$PATH

# Clojure
# See (http://riddell.us/ClojureSwankLeiningenWithEmacsOnLinux.html)
# 1. Install clojure-mode via git or marmalade-repo.org
# 2. lein plugin install swank-clojure 1.3.1
# 3. Invoke M-x clojure-jack-in from a project
#
export CLOJURE_EXT=~/.clojure
alias clj=clj-env-dir


