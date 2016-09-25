# .bash_profile  

# Get the aliases and functions

if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi

PATH=$PATH:$HOME/.local/bin:$HOME/bin
PATH=$PATH:/bin

# Java path
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
# Go path
export PATH=$PATH:/usr/local/go/bin
# Cask path 
export PATH="/home/gabriel/.cask/bin:$PATH"
# Cabal path
export PATH=$PATH:/home/gabriel/.cabal/bin

# Environment variables
EDITOR="vim"; export EDITOR
VISUAL="$EDITOR"; export VISUAL
ZSH_TMUX_AUTOSTART=true; export ZSH_TMUX_AUTOSTART
# Mount Drive
# gdfstool mount ~/data/gdrivefs/gdfs.creds ~/Drive/
# gdfstool mount ~/data/gdrivefs/gdfs2.creds ~/Drive_ST/

# Fix keyboard leyout
setxkbmap -model abnt2 -layout br -variant abnt2

# Change scape and CapsLock
setxkbmap -option caps:escape
