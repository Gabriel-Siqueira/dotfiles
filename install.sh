# install oh-my-zsh
if [ ! -d "$HOME/.oh-my-zsh/" ]
then
    echo "Installing: oh-my-zsh"
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
    git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
    rm -f ~/.zshrc
fi

# install spacemacs
if [ ! -f "$HOME/.spacemacs" ]
then
    echo "Installing: spacemacs"
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
    rm -f ~/.spacemacs
fi

# install spacevim
if [ ! -f "$HOME/.SpaceVim" ]
then
    echo "Installing: spacevim"
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
    rm -f ~/.SpaceVim.d
fi

# install tmux plugin manager
if [ ! -f "$HOME/.tmux/plugins/tpm" ]
then
    echo "Installing: tpm"
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

stow -v -R --ignore='.*~undo-tree~.*' -t ~ spacevim other shell spacemacs haskell xmonad

# make scripts executable
for i in ~/bin/*
do
    chmod +x $i
done
