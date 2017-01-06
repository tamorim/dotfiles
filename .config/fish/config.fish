# Path to Oh My Fish install.
set -q XDG_DATA_HOME
  and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
  or set -gx OMF_PATH "$HOME/.local/share/omf"

# Customize Oh My Fish configuration path.
# set -gx OMF_CONFIG "/home/thor/.config/omf"

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish
set fish_greeting ''
set -gx PATH ~/.npm-global/bin $PATH
set -gx EDITOR 'nvim'
set -gx VISUAL 'nvim'

alias tm='tmux attach -t base; or tmux new -s base'
for script in (ls ~/.scripts)
  set -l alias_name (echo $script | sed -n 's/^\(.*\)\.sh/\1/p')
  alias $alias_name=~/.scripts/$script
end

# Remember to change to your primary screen
# scrn primary LVDS1

[ $TERM != "screen-256color" ]; and [ ! $NVIM_TERM ]; and tm
