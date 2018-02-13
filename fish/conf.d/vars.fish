# Set up environment variables here
set BUILD_PREFIX $HOME/.local
set -x PATH $HOME/.bin $HOME/.bin/ngts $BUILD_PREFIX/bin $HOME/.cargo/bin $PATH
set EDITOR nvim
set VISUAL $EDITOR

# Set up fzf
set FZF_DEFAULT_OPTS "--color dark --tac --ansi --no-mouse --tabstop 4 --inline-info --tiebreak=begin,length"

# General settings
set TREE_CHARSET ascii               # For rendering with the `tree` command
