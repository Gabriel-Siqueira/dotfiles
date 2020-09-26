# Bin Paths
typeset -U path
path=(/bin /sbin ~/.local/bin ~/bin
      ~/.ghcup/bin
      ~/.cabal/bin
      ~/.gem/ruby/2.6.0/bin
      ~/.go/bin
      ~/.npm_global/bin/
	  ~/.Gurobi/bin
      $path)

# Variables
export FZF_BASE=~/.fzf.zsh
export EDITOR="nvim"
export VISUAL="nvim"
export MANPAGER="nvim -c 'set ft=man' -"

# Gurobi
export GUROBI_HOME="$HOME/.Gurobi/"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$GUROBI_HOME/lib"
export GRB_LICENSE_FILE="$GUROBI_HOME/gurobi.lic"

# Paths Variables
export TASKDATA="$HOME/Dropbox/Backup/pc/task/"
export MY_WIKI="$HOME/Dropbox/Wiki/"
export MY_LEDGER="$HOME/Dropbox/Personal/finance.ledger"
export MY_REFS="$HOME/Dropbox/Ref/pdfs/"
export MY_BIB="$HOME/Dropbox/Ref/better-ref.bib"
export MY_MACROS="$HOME/Dropbox/Backup/pc/xmacro/"
export MY_EMOJIS="$HOME/Dropbox/Backup/pc/emojis.txt"
export MY_OTHER_ACT="$HOME/Dropbox/Backup/pc/other_activities.txt"
export MY_REPORTS="$HOME/Dropbox/Backup/pc/reports"
export MY_VIMRC="$HOME/.config/nvim/init.vim"
export MY_ZSH="$HOME/.oh-my-zsh/custom/my_config.zsh"
export MY_XMONAD="$HOME/.xmonad/xmonad.hs"
