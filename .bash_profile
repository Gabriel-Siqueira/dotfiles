# .bash_profile  

# Get the aliases and functions

if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi

PATH=$PATH:$HOME/.local/bin:$HOME/bin

# java path
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
# go path
export PATH=$PATH:/usr/local/go/bin
# cask path 
export PATH="/home/gabriel/.cask/bin:$PATH"

# environment variables
EDITOR="vim"; export EDITOR
VISUAL="$EDITOR"; export VISUAL

#fix keyboard leyout
setxkbmap -model abnt2 -layout br -variant abnt2

# change scape and CapsLock
setxkbmap -option caps:escape
