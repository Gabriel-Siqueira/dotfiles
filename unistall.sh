#!/bin/bash

# echo "Uninstalling oh-my-zsh" && rm -rf ~/.oh_oh_my_zsh
# echo "Uninstalling tpm" && rm -f ~/.tmux/plugins/tpm 

stow -v -D --ignore='.*~undo-tree~.*' -t ~ other shell haskell
# mkdir -p ~/.xmonad && stow -v -R --ignore='.*~undo-tree~.*' -t ~/.xmonad xmonad
# mkdir -p ~/.SpaceVim.d && stow -v -R --ignore='.*~undo-tree~.*' -t ~/.SpaceVim.d spacevim
mkdir -p ~/.config && stow -D -v --ignore='.*~undo-tree~.*' -t ~/.config config
mkdir -p ~/bin && stow -D -v --ignore='.*~undo-tree~.*' -t ~/bin bin
