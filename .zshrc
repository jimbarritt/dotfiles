export PATH=$HOME/bin:/usr/local/bin:$PATH
export TERM=xterm-256color

autoload -Uz compinit
compinit


# Load local OpenAI key if present
if [ -f "$HOME/.config/.open_api_key" ]; then
  source "$HOME/.config/.open_api_key"
fi

export SITE_HOME=$HOME/Code/github/juxt-site/site
export PATH=$SITE_HOME/bin:$SITE_HOME/server/bin:$PATH
# Source site zshrc lazily: only source it when we `cd` into the site workspace or explicitly request it.
SITE_ZSHRC="$SITE_HOME/etc/zsh/zshrc"
if [ -f "$SITE_ZSHRC" ]; then
  _site_zshrc_sourced=0
  maybe_source_site_zshrc() {
    if [ "$_site_zshrc_sourced" -eq 0 ] 2>/dev/null && [[ "$PWD" == "$SITE_HOME"* ]]; then
      source "$SITE_ZSHRC"
      _site_zshrc_sourced=1
    fi
  }
  autoload -Uz add-zsh-hook
  add-zsh-hook chpwd maybe_source_site_zshrc
  # If we start inside the site, source it immediately
  if [[ "$PWD" == "$SITE_HOME"* ]]; then
    maybe_source_site_zshrc
  fi
  # provide a manual command to source it on demand
  source-site-zshrc() { [ -f "$SITE_ZSHRC" ] && source "$SITE_ZSHRC" && _site_zshrc_sourced=1; }
fi

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes

ZSH_THEME="green-tinted"
ZSH_DISABLE_COMPFIX="true"
VI_MODE_SET_CURSOR=true
VI_MODE_RESET_PROMPT_ON_MODE_CHANGE=true
# Path to your oh-my-zsh installation. Export early so we can probe plugin dirs.
export ZSH="$HOME/.oh-my-zsh"

# Which plugins would you like to load?
# We'll only enable plugins that actually exist to avoid oh-my-zsh warnings.
# Desired plugins list:
desired_plugins=(git zsh-syntax-highlighting zsh-autosuggestions, vi-mode)

# Build plugins array from available plugin directories
plugins=()
for p in "${desired_plugins[@]}"; do
  if [ -d "${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/$p" ] || [ -d "$ZSH/plugins/$p" ]; then
    plugins+=($p)
  fi
done

# Enable simple completion caching to speed up completion
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$HOME/.zsh/cache"
mkdir -p "$HOME/.zsh/cache"

# colors, a lot of colors!
function clicolors() {
    i=1
    for color in {000..255}; do;
        c=$c"$FG[$color]$color✔$reset_color  ";
        if [ `expr $i % 8` -eq 0 ]; then
            c=$c"\n"
        fi
        i=`expr $i + 1`
    done;
    echo $c | sed 's/%//g' | sed 's/{//g' | sed 's/}//g' | sed '$s/..$//';
    c=''
}



# Standard plugin installation is handled by oh-my-zsh; source it now.
source $ZSH/oh-my-zsh.sh

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=118'
# Ensure ZSH_HIGHLIGHT_STYLES is an associative array before assigning
if ! typeset -p ZSH_HIGHLIGHT_STYLES >/dev/null 2>&1; then
  typeset -A ZSH_HIGHLIGHT_STYLES
fi
ZSH_HIGHLIGHT_STYLES[comment]='fg=244'

# Vi-mode cursor configuration
# Blinking block in insert mode
VI_MODE_CURSOR_INSERT=1

# Steady block in normal mode  
VI_MODE_CURSOR_NORMAL=2

# Or use these custom functions for more control
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] || [[ $1 = 'block' ]]; then
    # Normal mode - steady block
    echo -ne '\e[2 q'
  elif [[ ${KEYMAP} == main ]] || [[ ${KEYMAP} == viins ]] || [[ $1 = 'beam' ]]; then
    # Insert mode - blinking block
    echo -ne '\e[1 q'
  fi
}
zle -N zle-keymap-select

# Start with blinking block (insert mode)
echo -ne '\e[1 q'

# Also reset cursor on new prompt
function zle-line-init {
  echo -ne '\e[1 q'
}
zle -N zle-line-init
bindkey -M viins '^G' vi-cmd-mode
# clicolors

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias site='docker exec -it $(docker ps -qf ancestor=site-server) site'
alias sitesh='docker exec -it $(docker ps -qf ancestor=site-server) bash'

alias dc="docker-compose"
alias ll="ls -larhtG"
alias ls="ls -G"

if [ -n "$INSIDE_EMACS" ]; then
    if [[ "$INSIDE_EMACS" = nil ]]; then
        print -P "ZSH inside emacs environment..."
        chpwd() { print -P "\033AnSiTc %d" }
        print -P "\033AnSiTu %n"
        print -P "\033AnSiTc %d"
    fi
fi

vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}


if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
    autoload -U add-zsh-hook
    add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

fi



ENABLE_CORRECTION="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="yyyy-mm-dd"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Enables directory tracking when running in emacs

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    vterm_prompt_end() {
        vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
    }
    setopt PROMPT_SUBST
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
fi



# Helpers: update and compile zsh plugins for faster startup
ZSH_PLUGINS_DIR="${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins"
_zsh_plugins_list=(zsh-syntax-highlighting zsh-autosuggestions)

update-zsh-plugins() {
  echo "Updating zsh plugins in: $ZSH_PLUGINS_DIR"
  for p in "${_zsh_plugins_list[@]}"; do
    if [ -d "$ZSH_PLUGINS_DIR/$p/.git" ]; then
      printf "\nUpdating %s...\n" "$p"
      git -C "$ZSH_PLUGINS_DIR/$p" pull --ff-only --recurse-submodules || git -C "$ZSH_PLUGINS_DIR/$p" fetch --all
    elif [ -d "$ZSH_PLUGINS_DIR/$p" ]; then
      printf "%s exists but is not a git repo (skipping)\n" "$p"
    else
      printf "%s is missing\n" "$p"
    fi
done
}

compile-zsh-plugins() {
  if ! command -v zcompile >/dev/null 2>&1; then
    echo "zcompile not available; skipping compilation"
    return 0
  fi
  echo "Compiling plugin scripts to .zwc where possible..."
  for p in "${_zsh_plugins_list[@]}"; do
    plugdir="$ZSH_PLUGINS_DIR/$p"
    if [ -d "$plugdir" ]; then
      # compile any .zsh/.sh/plugin script files we find
      for f in "$plugdir"/*.zsh "$plugdir"/*.sh "$plugdir"/*.plugin.zsh; do
        [ -f "$f" ] || continue
        printf "Compiling %s\n" "$f"
        zcompile -q "$f" 2>/dev/null || true
      done
    fi
done
}

export HOMEBREW_NO_ENV_HINTS=1

# Combined helper
zsh-plugins-setup() {
  update-zsh-plugins
  compile-zsh-plugins
}

# Provide a convenient alias to run the helpers quickly
alias zpsetup='zsh-plugins-setup'

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# pnpm
export PNPM_HOME="/Users/jmdb/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# Git prompt configuration - [branch] with bright branch name
ZSH_THEME_GIT_PROMPT_PREFIX="[%{$fg_bold[cyan]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}]"
ZSH_THEME_GIT_PROMPT_DIRTY=""
ZSH_THEME_GIT_PROMPT_CLEAN=""
  
PROMPT='%{$fg[green]%}%~ %{$fg[cyan]%}$(git_prompt_info)%{$reset_color%}
$ '

export SDKMAN_DIR=$(brew --prefix sdkman-cli)/libexec
[[ -s "${SDKMAN_DIR}/bin/sdkman-init.sh" ]] && source "${SDKMAN_DIR}/bin/sdkman-init.sh"

export PATH="$HOME/.local/bin:$PATH"

alias kotlin-app='gradle init --type kotlin-application --dsl kotlin'
