# Paths
typeset -U path
path=(/bin /sbin ~/.local/bin ~/bin
      ~/.ghcup/bin
      ~/.cabal/bin
      ~/.gem/ruby/2.6.0/bin
      ~/.go/bin
      ~/.npm_global/bin/
      $path)

# Variables
export FZF_BASE=~/.fzf.zsh
if ! type "emacsclient" > /dev/null; then
	EDITOR="vim"
    VISUAL="vim"
else
	EDITOR="emacsclient"
    VISUAL="emacsclient"
fi

# Key Maps
setxkbmap -layout us,br -variant dvp, -option "grp:shifts_toggle"
setxkbmap -option compose:102
xmodmap -e 'keycode 135 = Super_R'
xmodmap -e 'keycode 97 = Control_R'
