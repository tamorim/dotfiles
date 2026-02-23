setopt IGNORE_EOF
zstyle ':bracketed-paste-magic' active-widgets '.self-*'
zle_highlight+=(paste:none)

source <(fzf --zsh)

ANTIGEN_PATH=/opt/homebrew/share/antigen/antigen.zsh
[ -f $ANTIGEN_PATH ] && source $ANTIGEN_PATH
antigen init ~/.antigenrc

export DISABLE_AUTO_TITLE="true"
export EDITOR="nvim"
export VISUAL="nvim"
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --ignore-file ~/.agignore 2> /dev/null'

bindkey  backward-kill-line

[ $TERM != "screen-256color" -a ! $NVIM_TERM -a -x "$(command -v tmux)" ] && tmux new-session -A -s base

zsh_custom_file=~/.zsh_custom.sh
[ -e "$zsh_custom_file" ] && source $zsh_custom_file
