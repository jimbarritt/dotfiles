setopt prompt_subst
autoload -Uz vcs_info

zstyle ':vcs_info:git:*' formats '[%b]'

vcs_info_wrapper() {
  vcs_info
  if [ -n "$vcs_info_msg_0_" ]; then
    echo "%{$fg[magenta]%}${vcs_info_msg_0_}%{$reset_color%}$del"
  fi
}

precmd(){
    local preprompt_left="%F{cyan}%n@%m %F{green}%~ $(vcs_info_wrapper)"
    local preprompt_right=""
    local preprompt_left_length=${#${(S%%)preprompt_left//(\%([KF1]|)\{*\}|\%[Bbkf])}}
    local preprompt_right_length=${#${(S%%)preprompt_right//(\%([KF1]|)\{*\}|\%[Bbkf])}}
    local num_filler_spaces=$((COLUMNS - preprompt_left_length - preprompt_right_length))
    print -Pr $'\n'"$preprompt_left${(l:$num_filler_spaces:)}$preprompt_right"
}

PS1="$ "
PS2="$ "
RPS1=""


