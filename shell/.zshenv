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
