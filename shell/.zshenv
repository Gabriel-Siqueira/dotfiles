# Bin Paths
typeset -U path
path=(/bin /sbin /usr/bin/site_perl /usr/bin/vendor_perl /usr/bin/core_perl ~/.local/bin ~/bin
      ~/.ghcup/bin
      ~/.cabal/bin
      ~/.local/share/gem/ruby/3.0.0/bin
      ~/.go/bin
      ~/.npm_global/bin/
	  ~/.Gurobi/bin
	  ~/R/x86_64-pc-linux-gnu-library/4.0/irace/bin
      $path)

# Variables
export FZF_BASE=~/.fzf.zsh
export EDITOR="nvim"
export VISUAL="nvim"
export MANPAGER="nvim -c 'set ft=man' -"

# irace
export IRACE_HOME="$HOME/R/x86_64-pc-linux-gnu-library/4.0/irace"
export R_LIBS=${R_LIBS_USER}:${R_LIBS}

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
export MY_VSCODE="$HOME/.config/Code/User/settings.json"
export MY_ZSH="$HOME/.oh-my-zsh/custom/my_config.zsh"
export MY_XMONAD="$HOME/.xmonad/xmonad.hs"
export YGGDRASIL_PATH="$HOME/Dropbox/YGG_DB/"
