# Alias {{{
# programs {{{
if type "thefuck" > /dev/null; then
	eval $(thefuck --alias)
	alias f='fuck'
fi
if type "nvim" > /dev/null; then
	alias vim-old='vim'
	alias vim='nvim'
fi
if type "emacsclient" > /dev/null; then
	alias e='emacsclient'
fi
if type "xdg-open" > /dev/null; then
	alias o='xdg-open'
fi
if type "trizen" > /dev/null; then
  alias t='trizen'
fi
if type "lsd" > /dev/null; then
  alias ls='lsd'
else
  alias ls='ls --color=auto'
fi
# }}}
# exeptions to auto-correction {{{
alias mkdir='nocorrect mkdir -p'
alias mv='nocorrect mv -i'
# }}}
# usefull {{{
alias clr='clear'
alias gccs='gcc -ansi -pedantic -Wall -Werror -lm'
alias emacsc='emacsclient'
alias l="ls"
alias dot='ls .[a-zA-Z0-9_]*'
alias ll='ls -l'
alias la='ls -a'
alias lh="ls -lh"
alias ltr="ls -ltr"
alias cd..='cd ..'
alias rm='rm -i'
alias del='gio trash'
alias cp='cp -ai'
alias .='pwd'
alias ..='cd ..'
alias ...='cd ../..'
alias grep='grep --color=auto'
alias df='df -h'
alias df10='df -H'
alias du='du -h'
alias weather='curl http://wttr.in/'
# }}}
# scripts {{{
alias st=$HOME/bin/set_task.sh
alias ct="$HOME/bin/set_task.sh 0"
alias pla="$HOME/bin/report.py $MY_REPORTS/daily.txt"
# }}}
# }}}
# Global variables {{{
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=7'
LS_COLORS='di=1;33';export LS_COLORS
# manpage on nvim
# if ! type "nvim" > /dev/null; then
#     export MANPAGER="/bin/sh -c \"col -b | vim --not-a-term -c 'set ft=man ts=8 nomod nolist noma' -\""
# else
#     export MANPAGER="nvim +set\ filetype=man -"
# fi
# spark
export PYSPARK_DRIVER_PYTHON=ipython
# }}}
# Settings on history {{{
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt hist_ignore_all_dups
# }}}
# Hashs {{{
hash -d dow=~/Downloads
hash -d doc=~/Documents
hash -d dro=~/Dropbox
hash -d dri=~/Drive
hash -d ath=~/Dropbox/Projects/ath
hash -d chi=~/Dropbox/Projects/chi
hash -d maze=~/Dropbox/Projects/maze
hash -d hep=~/Dropbox/Projects/hep
hash -d ran=~/random
hash -d trash=~/.local/share/Trash/files
# }}}
# Settings {{{
setopt autocd  # auto use cd on files names and nomatch
setopt nomatch # unmatched patterns are left unchanged insted of error
unsetopt beep  # no beep
setopt interactivecomments # allow comments, even in interactive shells
# }}}
# Compinstall {{{
# The following lines were added by compinstall
zstyle ':completion:*' completer _complete _ignored _correct
zstyle ':completion:*' format 'Competing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=0
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' verbose true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle :compinstall filename "$HOME/.oh-my-zsh/custom/my_config.zsh"

autoload -Uz compinit
compinit
# End of lines added by compinstall
# }}}
# keys {{{
bindkey -v # vim keys
bindkey -s ^] '\e' # new escape
bindkey -a "!" sudo-command-line # add sudo on line
# }}}
# Others {{{
# Pyenv virtualenv
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# work nice with emacs
if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
  export ATHAME_ENABLED=0
  alias vim='emacsclient'
fi

if [ $(date +"%H") -ge 18 ]; then
    echo "Boa noite :o"
elif [ $(date +"%H") -ge 12 ]; then
    echo "Boa tarde :]"
else
    echo "Bom dia :)"
fi

# work nice with neovim
[ -n "$NVIM_LISTEN_ADDRESS" ] && export FZF_DEFAULT_OPTS='--no-height'

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[2 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[6 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    echo -ne "\e[6 q"
}
zle -N zle-line-init
echo -ne '\e[6 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[6 q' ;} # Use beam shape cursor for each new prompt.
# }}}
# Local {{{
if [ -f "./local.zsh" ]; then
    source local.zsh
fi
# }}}
# vim: foldmethod=marker foldlevel=0
# Local Variables:
# origami-fold-style: triple-braces
# End:
