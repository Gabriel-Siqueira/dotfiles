# .bashrc

#Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# ${EUID} identifica o tipo de usuario:
#    se for 0 eh o root, caso contrario, eh um usuario comum
# \[\033[x;xxm]] indica uma cor
# \w eh substituido pelo nome do diretorio atual
# \$ eh substituido por $ caso o usuario seja normal ou por # caso seja root
# \u eh substituido pelo nome do usuario

if [[ ${EUID} != 0 ]] ; then
    PS1='\[\033[0;33m\]\u\[\033[0;37m\]:>> '
else
    PS1='\[\033[1;31m\]\u\[\033[1;31m\]-\#\[\033[0;01m\]:# '
fi

# manda para lixeira
function trash(){
	n=$1;
	mv $n ~/.local/share/Trash/files/;
}

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
alias programingtime="echo 'let\'s have some fun'"

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

# quando usamos "seta para cima" para visualizar o historico de
# comandos, essa especificacao faz com que comandos iguais sejam
# ignorados

HISTCONTROL=ignoreboth
export HISTCONTROL

export PATH=/home/gsiqueira/applications/sbt/usr/bin:$PATH
export PATH=/usr/java/jdk1.8.0_31/bin:$PATH

# gitprompt configuration

# Set config variables first
GIT_PROMPT_ONLY_IN_REPO=1

# GIT_PROMPT_FETCH_REMOTE_STATUS=0   # uncomment to avoid fetching remote status

#GIT_PROMPT_START='\[\033[0;33m\]\u\[\033[0;37m\]:>> '   # uncomment for custom prompt start sequence
# GIT_PROMPT_END=...      # uncomment for custom prompt end sequence

# as last entry source the gitprompt script
# GIT_PROMPT_THEME=Custom # use custom .git-prompt-colors.sh
# GIT_PROMPT_THEME=Solarized # use theme optimized for solarized color scheme
source ~/.bash-git-prompt/gitprompt.sh
