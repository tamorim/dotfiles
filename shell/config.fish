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
