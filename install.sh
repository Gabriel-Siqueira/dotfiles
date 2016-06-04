# create links to use files in .dotfiles folder
ln -s ~/.dotfiles/.bash_profile       ~/.bash_profile
ln -s ~/.dotfiles/.bash_profile       ~/.profile
ln -s ~/.dotfiles/.bashrc             ~/.bashrc

ln -s ~/.dotfiles/zsh/.zshrc          ~/.zshrc
ln -s ~/.dotfiles/zsh/my.zsh-theme    ~/.oh-my-zsh/themes/my.zsh-theme
ln -s ~/.dotfiles/zsh/my_config.zsh   ~/.oh-my-zsh/custom/my_config.zsh

ln -s ~/.dotfiles/editors/.emacs.d/emacs.org  ~/.emacs.d/emacs.org
ln -s ~/.dotfiles/editors/.emacs.d/init.el    ~/.emacs.d/init.el
ln -s ~/.dotfiles/editors/.emacs.d/mysnippets ~/.emacs.d/mysnippets

ln -s ~/.dotfiles/editors/.inputrc            ~/.inputrc

ln -s ~/.dotfiles/editors/.vimrc       ~/.vimrc
ln -s ~/.dotfiles/.vimperatorrc        ~/.vimperatorrc
ln -s ~/.dotfiles/.cvimrc              ~/.cvimrc

ln -s ~/.dotfiles/wm/awesome/rc.lua    ~/.config/awesome/rc.lua

ln -s ~/.dotfiles/wm/.i3/config        ~/.i3/config

ln -s ~/.dotfiles/wm/xmonad/xmonad.hs  ~/.xmonad/xmonad.hs
ln -s ~/.dotfiles/wm/xmonad/.xmobarrc  ~/.xmobarrc

ln -s ~/.dotfiles/.stalonetrayrc       ~/.stalonetrayrc
