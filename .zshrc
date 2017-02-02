source ~/.fzf.zsh
source /usr/share/zsh/scripts/antigen/antigen.zsh
antigen init ~/.antigenrc

export DISABLE_AUTO_TITLE="true"
export PATH=~/.npm-global/bin:$PATH
export EDITOR="nvim"
export VISUAL="nvim"

for script in $(ls ~/.scripts); do
  alias_name=$(echo $script | sed -n 's/^\(.*\)\.sh/\1/p')
  alias $alias_name=~/.scripts/$script
done

# Remember to change to your primary screen
# scrn primary LVDS1

[[ $TERM != "screen-256color" && ! $NVIM_TERM ]] && tmux attach -t base
