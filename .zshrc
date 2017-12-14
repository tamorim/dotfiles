setopt IGNORE_EOF
zstyle ':bracketed-paste-magic' active-widgets '.self-*'
zle_highlight+=(paste:none)

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
source /usr/share/antigen.zsh
antigen init ~/.antigenrc

export DISABLE_AUTO_TITLE="true"
export PATH=~/.npm-global/bin:$PATH
export EDITOR="nvim"
export VISUAL="nvim"
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --ignore-file ~/.agignore 2> /dev/null'

bindkey  backward-kill-line

if [ $TERM != "screen-256color" -a ! $NVIM_TERM ]; then
  default_session="base"
  has_session=$(tmux has-session -t $default_session 2>/dev/null && echo 1)
  if [ $has_session ]; then
    tmux attach -t $default_session
  else
    tmux new-session -s $default_session
  fi
fi

zsh_custom_file=~/.zsh_custom.sh
[ -e "$zsh_custom_file" ] && source $zsh_custom_file
