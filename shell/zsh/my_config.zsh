PATH=$PATH:/bin
# Cabal path
PATH=$PATH:/home/gabriel/.cabal/bin

# Functions

# Use ssh on ic
function ic_term(){
	ssh ra155446@ssh.students.ic.unicamp.br
}

alias oi="echo 'oi, tenha um bom dia'"
alias tudoerrado="echo 'não desista as coisas vão dar certo'"
alias culpasua="echo 'não é a maquina que comete erros é o programador'"
alias socorro="echo 'continue a nadar'"
alias resposta="echo '42'"
alias answer="echo '42'"
alias frase="echo 'We live in a world of possibilities'"
alias programingtime="echo 'let´s have some fun'"

alias clr='clear'
alias gccs='gcc -ansi -pedantic -Wall -Werror -lm'
alias emacsc='emacsclient'
alias l="ls"
alias ls='ls --color=auto'
alias dot='ls .[a-zA-Z0-9_]*'
alias ll='ls -l'
alias la='ls -A'
alias lh="ls -lh"
alias ltr="ls -ltr"
alias cd..='cd ..'
alias mkdir='mkdir -p'
alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -ai'
alias .='pwd'
alias ..='cd ..'
alias ...='cd ../..'
alias grep='grep --color=auto'
alias df='df -h'
alias df10='df -H'
alias du='du -h'

# settings on history
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt hist_ignore_all_dups

# auto use cd on files names and nomatch
setopt autocd nomatch
# no beep
unsetopt beep notify

bindkey -v

# work nice with emacs
if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
  export ATHAME_ENABLED=0
  bindkey -e
fi

# use fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# screenfetch (begin)
screenfetch

# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored _correct
zstyle ':completion:*' format 'Competing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=0
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/home/gabriel/.oh-my-zsh/custom/my_config.zsh'

autoload -Uz compinit
compinit
# End of lines added by compinstall