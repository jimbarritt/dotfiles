# Green Tinted Zsh Theme - Colors Only
# Add this to your ~/.zshrc

autoload -U colors && colors

if [[ "$OSTYPE" == "linux-gnu"* ]] || [[ "$OSTYPE" == "freebsd"* ]]; then
    export LS_COLORS='di=38;5;77:ln=38;5;78:so=38;5;71:pi=38;5;114:ex=38;5;120:bd=38;5;77:cd=38;5;77:su=38;5;120:sg=38;5;120:tw=38;5;77:ow=38;5;77:*.tar=38;5;71:*.zip=38;5;71:*.gz=38;5;71'
    alias ls='ls --color=auto'
elif [[ "$OSTYPE" == "darwin"* ]]; then
    export CLICOLOR=1
    export LSCOLORS="CxGxFxdxGxDxDxhbhdCxCx"
fi

export GREP_COLOR='38;5;78'
alias grep='grep --color=auto'

export LESS_TERMCAP_mb=$(printf '\e[38;5;78m')
export LESS_TERMCAP_md=$(printf '\e[1;38;5;77m')
export LESS_TERMCAP_me=$(printf '\e[0m')
export LESS_TERMCAP_se=$(printf '\e[0m')
export LESS_TERMCAP_so=$(printf '\e[38;5;114m')
export LESS_TERMCAP_ue=$(printf '\e[0m')
export LESS_TERMCAP_us=$(printf '\e[38;5;78m')