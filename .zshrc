# manda para lixeira
function trash(){
	n=$1;
	mv $n ~/.local/share/Trash/files/;
}

function ic_term(){
	ssh ra155446@ssh.students.ic.unicamp.br
}

if [ ${EUID} != 0 ] ; then
	PROMPT="%F{gray}┌╼%f%F{yellow} ⬖ -(%f%F{green}%~%f%F{yellow})-⬗  %D{%d/%m/%y} %f%F{gray}╾╼ %f%F{yellow}%T%f%F{gray}
└╼ %f%F{yellow}%n%f%F{gray}:>> $reset_color"
	RPROMPT="%F{yellow}[%f%F{green}%M %f%F{yellow}| %f%F{green}%l %f%F{yellow}| %f%F{green}%!%f%F{yellow}]"
else
	PROMPT="%F{gray}┌╼%f%F{red} ⬖ -(%f%F{white}%~%f%F{red})-⬗  %D{%d/%m/%y} %f%F{gray}╾╼ %f%F{red}%T%f%F{gray}
└╼ %f%F{red}%n%f%F{gray}:# $reset_color"
	RPROMPT="%F{red}[%f%F{white}%M %f%F{red}| %f%F{white}%l %f%F{red}| %f%F{white}%!%f%F{red}]"
fi

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
# historico nao eh sobrescrito
setopt appendhistory

# auto use cd on files names and nomatch
setopt autocd nomatch
# no beep
unsetopt beep notify

# Completition settings
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list '+' 'm:{[:lower:]}={[:upper:]} r:|[. _ -]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt "%p/%l >"

autoload -Uz compinit
compinit

# add colors
export TERM=xterm-256color

# work nice with emacs
if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
  set key -o emacs
fi
set key -o emacs

# screenfetch (begin)
screenfetch
