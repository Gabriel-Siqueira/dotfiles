# create links to use files in .dotfiles folder

## Bash
# rm -i ~/.bash_profile
# ln -s ~/MEGA/dotfiles/bash_profile       ~/.bash_profile
# rm -i ~/.profile
# ln -s ~/MEGA/dotfiles/bash_profile       ~/.profile
# rm -i ~/.bashrc
# ln -s ~/MEGA/dotfiles/shell/bashrc       ~/.bashrc

## Zsh
# rm -i ~/.zshrc
# ln -s ~/MEGA/dotfiles/shell/zsh/zshrc           ~/.zshrc
# rm -i ~/.oh-my-zsh/themes/my.zsh-theme
# ln -s ~/MEGA/dotfiles/shell/zsh/my.zsh-theme    ~/.oh-my-zsh/themes/my.zsh-theme
# rm -i ~/.oh-my-zsh/custom/my_config.zsh
# ln -s ~/MEGA/dotfiles/shell/zsh/my_config.zsh   ~/.oh-my-zsh/custom/my_config.zsh

## Fish
# rm -i ~/.config/fish/config.fish
# ln -s ~/MEGA/dotfiles/shell/config.fish         ~/.config/fish/config.fish

## Emacs
# rm -i ~/.emacs.d/emacs.org
# ln -s ~/MEGA/dotfiles/editors/emacs.d/emacs.org  ~/.emacs.d/emacs.org
# rm -i ~/.emacs.d/init.el
# ln -s ~/MEGA/dotfiles/editors/emacs.d/init.el    ~/.emacs.d/init.el
# rm -i ~/.emacs.d/mysnippets
# ln -s ~/MEGA/dotfiles/editors/emacs.d/mysnippets ~/.emacs.d/mysnippets

## Read line
# rm -i ~/.inputrc
# ln -s ~/MEGA/dotfiles/editors/inputrc              ~/.inputrc

## Vim
# rm -i ~/.vimrc
# ln -s ~/MEGA/dotfiles/editors/vimrc                ~/.vimrc
# rm -i ~/.config/nvim/init.vim
# ln -s ~/MEGA/dotfiles/editors/vimrc                ~/.config/nvim/init.vim
# rm -i ~/.vim/UltiSnips
# ln -s ~/MEGA/dotfiles/editors/mysnippets       ~/.vim/UltiSnips
# rm -i ~/.config/nvim/UltiSnips
# ln -s ~/MEGA/dotfiles/editors/mysnippets       ~/.config/nvim/UltiSnips

## Vim browser
# rm -i ~/.vimperatorrc
# ln -s ~/MEGA/dotfiles/browser_vim/vimperatorrc     ~/.vimperatorrc
# rm -i ~/.vimperator/colors/dark.vimp
# ln -s ~/MEGA/dotfiles/browser_vim/dark.vimp        ~/.vimperator/colors/dark.vimp
# rm -i ~/.cvimrc
# ln -s ~/MEGA/dotfiles/browser_vim/cvimrc          ~/.cvimrc

## Xmonad
# rm -i ~/.xmonad/xmonad.hs
# ln -s ~/MEGA/dotfiles/wm/xmonad/xmonad.hs  ~/.xmonad/xmonad.hs
# rm -i ~/.xmobarrc
# ln -s ~/MEGA/dotfiles/wm/xmonad/xmobarrc   ~/.xmobarrc

## i3 status conf
# rm -i ~/.i3status.conf
# ln -s ~/MEGA/dotfiles/wm/i3/i3status.conf   ~/.i3status.conf

## i3 - polibar
rm -i ~/.config/polybar/config
ln -s ~/MEGA/dotfiles/wm/i3/polybar.conf ~/.config/polybar/config

## stalonetrayrc 
# rm -i ~/.stalonetrayrc
# ln -s ~/MEGA/dotfiles/stalonetrayrc    ~/.stalonetrayrc

## Terminals
# rm -i ~/.config/termite/config
# ln -s ~/MEGA/dotfiles/terminals/termite.cfg         ~/.config/termite/config
# rm -i ~/.Xresources
# ln -s ~/MEGA/dotfiles/terminals/Xresources          ~/.Xresources
# rm -i ~/.tmux.conf
# ln -s ~/MEGA/dotfiles/shell/tmux.conf     ~/.tmux.conf

## ranger
# rm -i ~/.config/ranger/rc.conf
# ln -s ~/MEGA/dotfiles/term_prog/ranger/rc.conf ~/.config/ranger/rc.conf
# rm -i ~/.config/ranger/rifle.conf
# ln -s ~/MEGA/dotfiles/term_prog/ranger/rifle.conf ~/.config/ranger/rifle.conf

## Scripts
# rm -i ~/bin/bat.sh
# ln -s ~/MEGA/dotfiles/bin/bat.sh ~/bin/bat.sh
# chmod +x ~/bin/bat.sh
# rm -i ~/bin/conky-i3bar
# ln -s ~/MEGA/dotfiles/bin/conky-i3bar ~/bin/conky-i3bar
# chmod +x ~/bin/conky-i3bar
# rm -i ~/bin/polybar.sh
# ln -s ~/MEGA/dotfiles/bin/polybar.sh ~/bin/polybar.sh
# chmod +x ~/bin/polybar.sh
