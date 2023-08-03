#!/bin/bash

echo "Installing oh-my-zsh" && bash installs/oh_my_zsh.sh
# echo "Installing spacevim" && bash installs/spacevim.sh
# echo "Installing tpm" && bash installs/tpm.sh

stow -v -R --ignore='.*~undo-tree~.*' -t ~ stow/other stow/shell stow/haskell
# mkdir -p ~/.xmonad && stow -v -R --ignore='.*~undo-tree~.*' -t ~/.xmonad stow/xmonad
# mkdir -p ~/.SpaceVim.d && stow -v -R --ignore='.*~undo-tree~.*' -t ~/.SpaceVim.d stow/spacevim
# mkdir -p ~/.config && stow -v -R --ignore='.*~undo-tree~.*' -t ~/.config stow/config
mkdir -p ~/bin && stow -v -R --ignore='.*~undo-tree~.*' -t ~/bin bin

echo "make scripts executable"
chmod +x install.sh
for i in ~/bin/*
do
   chmod +x $i
done
