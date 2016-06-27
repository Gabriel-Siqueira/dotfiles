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

# Environment variables
EDITOR="vim"; export EDITOR
VISUAL="$EDITOR"; export VISUAL

# Mount Drive
gdfstool mount ~/data/gdrivefs/gdfs.creds ~/Drive/
gdfstool mount ~/data/gdrivefs/gdfs2.creds ~/Drive_ST/

# Fix keyboard leyout
setxkbmap -model abnt2 -layout br -variant abnt2

# Change scape and CapsLock
setxkbmap -option caps:escape
