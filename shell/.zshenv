# Bin Paths
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
export EDITOR="nvim"
export VISUAL="nvim"
export MANPAGER="nvim -c 'set ft=man' -"

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
