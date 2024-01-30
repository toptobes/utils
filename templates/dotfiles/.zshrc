alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

mkcd()
{
  mkdir -p -- "$1" && cd -P -- "$_"
}

alias ..='cd ..'

alias tp='trash-put'
alias cl='clear'

alias wget-to='wget --output-document'

[ -f "/Users/kavin.gupta/.ghcup/env" ] && source "/Users/kavin.gupta/.ghcup/env" # ghcup-env

autoload -U colors && colors
autoload -Uz vcs_info
precmd() { vcs_info }

zstyle ':vcs_info:git:*' formats '%b '
setopt PROMPT_SUBST

PROMPT="%{$fg[green]%}me%{$reset_color%} %{$fg[yellow]%}%(5~|%-1~/.../%3~|%4~) %{$reset_color%}%F{red}\${vcs_info_msg_0_}%f\$ "

alias tree='tree -C'

autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
