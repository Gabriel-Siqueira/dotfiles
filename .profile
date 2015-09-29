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

# environment variables
EDITOR="emacs"; export EDITOR
VISUAL="$EDITOR"; export VISUAL

#fix keyboard leyout
setxkbmap -model abnt2 -layout br -variant abnt2

# change scape and CapsLock
# setxkbmap -option caps:escape
xmodmap -e 'clear Lock' #ensures you're not stuck in CAPS on mode
xmodmap -e 'keycode 0x66=Escape' #remaps the keyboard so CAPS LOCK=ESC
xmodmap -e 'keycode 0x9=Caps_Lock' #remaps the keyboard so ESC=CAPS LOCK
