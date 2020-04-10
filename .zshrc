# Lines configured by zsh-newuser-install
HISTFILE=~/.cache/.zsh_histfile
HISTSIZE=1000
SAVEHIST=1000
setopt extendedglob notify
unsetopt autocd beep nomatch
# bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/rodrigo/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Sets the prompt theme
# prompt walters

alias ls='ls --color'
alias grep='grep -n --color'

# Keybinds
typeset -g -A key

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"

# setup key accordingly
[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"      beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"       end-of-line

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
	autoload -Uz add-zle-hook-widget
	function zle_application_mode_start { echoti smkx }
	function zle_application_mode_stop { echoti rmkx }
	add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
	add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi

# --- END Keybinds ---

# Powerline-go
function powerline_precmd() {
    eval "$($GOPATH/bin/powerline-go -error $? -shell zsh -cwd-mode dironly -eval -modules venv,ssh,cwd,perms,hg,jobs,exit,root -modules-right git)"
}

function install_powerline_precmd() {
  for s in "${precmd_functions[@]}"; do
    if [ "$s" = "powerline_precmd" ]; then
      return
    fi
  done
  precmd_functions+=(powerline_precmd)
}

if [ "$TERM" != "linux" ]; then
    install_powerline_precmd
fi
# --- End Powerline-go ---

# Dotfiles config
alias config='/usr/bin/git --git-dir=/home/rodrigo/.dotfiles/ --work-tree=/home/rodrigo'

