setopt IGNORE_EOF

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
source /usr/share/antigen.zsh
antigen init ~/.antigenrc

export DISABLE_AUTO_TITLE="true"
export PATH=~/.npm-global/bin:$PATH
export EDITOR="nvim"
export VISUAL="nvim"
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --ignore-file ~/.agignore 2> /dev/null'

if [ $TERM != "screen-256color" -a ! $NVIM_TERM ]; then
  has_session=$(tmux has-session -t base 2>/dev/null && echo 1)
  if [ $has_session ]; then
    tmux attach -t base
  else
    tmux new-session -s base
  fi
fi

zsh_custom_file=~/.zsh_custom.sh
[ -e "$zsh_custom_file" ] && source $zsh_custom_file
