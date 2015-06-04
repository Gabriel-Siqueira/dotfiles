# .bash_profile  

# Get the aliases and functions

if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi

PATH=$PATH:$HOME/.local/bin:$HOME/bin

# go path
export PATH=$PATH:/usr/local/go/bin

# change scape and CapsLocki
xmodmap ~/.speedswapper
