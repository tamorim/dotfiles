setopt IGNORE_EOF

source ~/.fzf.zsh
source /usr/share/zsh/scripts/antigen/antigen.zsh
antigen init ~/.antigenrc

export DISABLE_AUTO_TITLE="true"
export PATH=~/.npm-global/bin:$PATH
export EDITOR="nvim"
export VISUAL="nvim"

source ~/.scripts/bootstrap.sh

[ $TERM != "screen-256color" ] && [ ! $NVIM_TERM ] && tmux attach -t base

