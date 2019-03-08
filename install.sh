i3=true
# i3=false
files=(
    bat.sh
    # conky_i3bar.sh
    polybar.sh
    # random_wallpaper.sh
    spacemacs
    private
    vimrc
    nvimrc
    ranger
    rifle
    Xresources
    # neomuttrc
    qutebrowser
    # snclirc
    termite
    zathurarc
    # bash_profile
    bashrc
    zsh_theme
    zsh_config
    zshrc
    zshenv
    # fish
    inputrc
    tmux
    polybar
    # i3status
)

declare -A from=(
    [bat.sh]="$PWD/bin/bat.sh"
    [conky_i3bar.sh]="$PWD/bin/conky_i3bar.sh"
    [polybar.sh]="$PWD/bin/polybar.sh"
    [random_wallpaper.sh]="$PWD/bin/random_wallpaper.sh"
    [spacemacs]="$PWD/editors/spacemacs/spacemacs"
    [private]="$PWD/editors/spacemacs/private"
    [vimrc]="$PWD/editors/vim/vimrc"
    [nvimrc]="$PWD/editors/vim/vimrc"
    [ranger]="$PWD/outros/ranger/rc.conf"
    [rifle]="$PWD/outros/ranger/rifle.conf"
    [Xresources]="$PWD/outros/Xresources"
    [neomuttrc]="$PWD/outros/neomuttrc"
    [qutebrowser]="$PWD/outros/qutebrowser.py"
    [snclirc]="$PWD/outros/snclirc"
    [termite]="$PWD/outros/termite.cfg"
    [zathurarc]="$PWD/outros/zathurarc"
    [bash_profile]="$PWD/shell/bash/bash_profile"
    [bashrc]="$PWD/shell/bash/bashrc"
    [zsh_theme]="$PWD/shell/zsh/my.zsh-theme"
    [zsh_config]="$PWD/shell/zsh/my_config.zsh"
    [zshrc]="$PWD/shell/zsh/zshrc"
    [zshenv]="$PWD/shell/zsh/zshenv"
    [fish]="$PWD/shell/config.fish"
    [inputrc]="$PWD/shell/inputrc"
    [tmux]="$PWD/shell/tmux.conf"
    [polybar]="$PWD/wm/i3/polybar.conf"
    [i3status]="$PWD/wm/i3/i3status.conf"
)

declare -A to=(
    [bat.sh]="$HOME/bin/bat.sh"
    [conky_i3bar.sh]="$HOME/bin/conky_i3bar.sh"
    [polybar.sh]="$HOME/bin/polybar.sh"
    [random_wallpaper.sh]="$HOME/bin/random_wallpaper.sh"
    [spacemacs]="$HOME/.spacemacs"
    [snippets]="$HOME/.emacs.d/private/snippets"
    [private]="$HOME/.emacs.d/private"
    [vimrc]="$HOME/.vimrc"
    [nvimrc]="$HOME/.config/nvim/init.vim"
    [ranger]="$HOME/.config/ranger/rc.conf"
    [rifle]="$HOME/.config/ranger/rifle.conf"
    [Xresources]="$HOME/.Xresources"
    [neomuttrc]="$HOME/.config/neomutt/neomuttrc"
    [qutebrowser]="$HOME/.config/qutebrowser/config.py"
    [snclirc]="$HOME/.config/.snclirc"
    [termite]="$HOME/.config/termite/config"
    [zathurarc]="$HOME/.config/zathura/zathurarc"
    [bash_profile]="$HOME/.bash_profile"
    [bashrc]="$HOME/.bashrc"
    [zsh_theme]="$HOME/.oh-my-zsh/themes/my.zsh-theme"
    [zsh_config]="$HOME/.oh-my-zsh/custom/my_config.zsh"
    [zshrc]="$HOME/.zshrc"
    [zshenv]="$HOME/.zshenv"
    [fish]="$HOME/.config/fish/config.fish"
    [inputrc]="$HOME/.inputrc"
    [tmux]="$HOME/.tmux.conf"
    [polybar]="$HOME/.config/polybar/config"
    [i3status]="$HOME/.i3status.conf"
)

# create links to use files in .dotfiles folder
for i in "${files[@]}";
do
    echo "linking $i"
    base=$(dirname "${to[$i]}")
    mkdir -p $base
    if [ -L "${to[$i]}" ]; then
        rm -f "${to[$i]}"
    elif [ -e "${to[$i]}" ]; then
        rm -ri "${to[$i]}"
    fi
    if [ ! -e "${to[$i]}" ]; then
        ln -s "${from[$i]}" "${to[$i]}"
    fi
done

# make shell scripts executable
if [ -f "~/bin/bat.sh" ]
then
    chmod +x ~/bin/bat.sh
fi
if [ -f "~/bin/conky_i3bar.sh" ]
then
    chmod +x ~/bin/conky_i3bar.sh
fi
if [ -f "~/bin/polybar.sh" ]
then
    chmod +x ~/bin/polybar.sh
fi
if [ -f "~/bin/random_wallpaper.sh" ]
then
    chmod +x ~/bin/random_wallpaper.sh
fi

# create i3 config file
mkdir -p $HOME/.config/i3
if $i3; then
    echo "Generating i3 config"
    python $PWD/wm/i3/i3config.py
fi
