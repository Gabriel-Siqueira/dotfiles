# create links to use files in .dotfiles folder

## editors {{{
## emacs {{{
# rm -i ~/.emacs.d/init-org.org
# ln -s ~/MEGA/dotfiles/editors/emacs.d/init-org.org  ~/.emacs.d/init-org.org
# rm -i ~/.emacs.d/init.el
# cp ~/MEGA/dotfiles/editors/emacs.d/init.el    ~/.emacs.d/init.el
# rm -i ~/.emacs.d/mysnippets
# ln -s ~/MEGA/dotfiles/editors/emacs.d/mysnippets ~/.emacs.d/mysnippets
## }}}
## vim {{{
# rm -i ~/.vimrc
# ln -s ~/MEGA/dotfiles/editors/vimrc                ~/.vimrc
# rm -i ~/.config/nvim/init.vim
# ln -s ~/MEGA/dotfiles/editors/vimrc                ~/.config/nvim/init.vim
# rm -i ~/.vim/UltiSnips
# ln -s ~/MEGA/dotfiles/editors/mysnippets       ~/.vim/UltiSnips
# rm -i ~/.config/nvim/UltiSnips
# ln -s ~/MEGA/dotfiles/editors/mysnippets       ~/.config/nvim/UltiSnips
## }}}
## read line {{{
# rm -i ~/.inputrc
# ln -s ~/MEGA/dotfiles/editors/inputrc              ~/.inputrc
## }}}
## }}}
## shell {{{
## bash {{{
# rm -i ~/.bash_profile
# ln -s ~/MEGA/dotfiles/shell/bash/bash_profile       ~/.bash_profile
# rm -i ~/.profile
# ln -s ~/MEGA/dotfiles/shell/bash/bash_profile       ~/.profile
# rm -i ~/.bashrc
# ln -s ~/MEGA/dotfiles/shell/bash/bashrc       ~/.bashrc
## }}}
## zsh {{{
# rm -i ~/.zshrc
# ln -s ~/MEGA/dotfiles/shell/zsh/zshrc           ~/.zshrc
# rm -i ~/.oh-my-zsh/themes/my.zsh-theme
# ln -s ~/MEGA/dotfiles/shell/zsh/my.zsh-theme    ~/.oh-my-zsh/themes/my.zsh-theme
# rm -i ~/.oh-my-zsh/custom/my_config.zsh
# ln -s ~/MEGA/dotfiles/shell/zsh/my_config.zsh   ~/.oh-my-zsh/custom/my_config.zsh
## }}}
## fish {{{
# rm -i ~/.config/fish/config.fish
# ln -s ~/MEGA/dotfiles/shell/config.fish         ~/.config/fish/config.fish
## }}}
## }}}
## prog {{{
## ranger {{{
# rm -i ~/.config/ranger/rc.conf
# ln -s ~/MEGA/dotfiles/prog/ranger/rc.conf ~/.config/ranger/rc.conf
# rm -i ~/.config/ranger/rifle.conf
# ln -s ~/MEGA/dotfiles/prog/ranger/rifle.conf ~/.config/ranger/rifle.conf
## }}}
# rm -i ~/.config/.snclirc
# ln -s ~/MEGA/dotfiles/prog/snclirc ~/.snclirc
# rm -i ~/.config/neomutt/neomuttrc
# ln -s ~/MEGA/dotfiles/prog/neomuttrc ~/.config/neomutt/neomuttrc
# rm -i ~/.config/zathura/zathurarc
# ln -s ~/MEGA/dotfiles/prog/zathurarc ~/.config/zathura/zathurarc
# rm -i ~/.config/qutebrowser/config.py
# ln -s ~/MEGA/dotfiles/prog/qutebrowser/config.py ~/.config/qutebrowser/config.py
## }}}
## wm {{{
## xmonad {{{
# rm -i ~/.xmonad/xmonad.hs
# ln -s ~/MEGA/dotfiles/wm/xmonad/xmonad.hs  ~/.xmonad/xmonad.hs
# rm -i ~/.xmobarrc
# ln -s ~/MEGA/dotfiles/wm/xmonad/xmobarrc   ~/.xmobarrc
## }}}
## i3 (run the haskell code ) {{{
# rm -i ~/.i3status.conf
# ln -s ~/MEGA/dotfiles/wm/i3/i3status.conf   ~/.i3status.conf
# rm -i ~/.conkyrc
# ln -s ~/MEGA/dotfiles/wm/i3/conkyrc   ~/.conkyrc
# rm -i ~/.config/polybar/config
# ln -s ~/MEGA/dotfiles/wm/i3/polybar.conf ~/.config/polybar/config
## }}}
## }}}
## Stalonetrayrc  {{{
# rm -i ~/.stalonetrayrc
# ln -s ~/MEGA/dotfiles/stalonetrayrc    ~/.stalonetrayrc
## }}}
## Terminals {{{
# rm -i ~/.config/termite/config
# ln -s ~/MEGA/dotfiles/terminals/termite.cfg         ~/.config/termite/config
# rm -i ~/.Xresources
# ln -s ~/MEGA/dotfiles/terminals/Xresources          ~/.Xresources
# rm -i ~/.tmux.conf
# ln -s ~/MEGA/dotfiles/shell/tmux.conf     ~/.tmux.conf
## }}}
## bin {{{
## low battery warning
# rm -i ~/bin/bat.sh
# ln -s ~/MEGA/dotfiles/bin/bat.sh ~/bin/bat.sh
# chmod +x ~/bin/bat.sh
## Conky as i3-bar data generator
# rm -i ~/bin/conky-i3bar
# ln -s ~/MEGA/dotfiles/bin/conky-i3bar ~/bin/conky-i3bar
# chmod +x ~/bin/conky-i3bar
## Start polybar
# rm -i ~/bin/polybar.sh
# ln -s ~/MEGA/dotfiles/bin/polybar.sh ~/bin/polybar.sh
# chmod +x ~/bin/polybar.sh
## random wallpapers
# rm -i ~/bin/ramdom_wallpaper.sh
# ln -s ~/MEGA/dotfiles/bin/random_wallpaper.sh ~/bin/ramdom_wallpaper.sh
# chmod +x ~/bin/ramdom_wallpaper.sh
## }}}

# vim: foldmethod=marker foldlevel=0
