# Paths {{{
PATH=$PATH:/bin
PATH=$PATH:/sbin
PATH=$PATH:$HOME/.local/bin
# Cabal path
PATH=$PATH:$HOME/.cabal/bin
# }}}
# Functions {{{
# Save/Load tmux session with tmuxp
function tmux_save(){
    rm ~/.tmuxp/default.yaml
    rm ~/MEGA/backup/default.yaml
    tmuxp freeze default
    mv ~/.tmuxp/default.yaml ~/MEGA/backup/tmuxp/default.yaml
}
function tmux_load(){
    tmuxp load ~/MEGA/backup/tmuxp/default.yaml
}
# Use ssh on ic
function ic_term(){
	ssh ra155446@ssh.students.ic.unicamp.br
}
# }}}
# Alias {{{
# programs {{{
alias betty="~/application/betty/main.rb"
eval $(thefuck --alias)
# }}}
# exeptions to auto-correction {{{
alias mkdir='nocorrect mkdir -p'
alias mv='nocorrect mv -i'
# }}}
# files {{{
alias fvim="vim ~/.vimrc"
alias fnvim="vim ~/.config/nvim/init.vim"
alias fzsh="vim ~/.oh-my-zsh/custom/my_config.zsh"
alias ftmux="vim ~/.tmux.conf"
alias fxmonad="vim ~/.xmonad/xmonad.hs"
alias fi3="vim ~/MEGA/dotfiles/wm/i3/i3config.hs"
alias nfvim="nvim ~/.vimrc"
alias nfnvim="nvim ~/.config/nvim/init.vim"
alias nfzsh="nvim ~/.oh-my-zsh/custom/my_config.zsh"
alias nftmux="nvim ~/.tmux.conf"
alias nfxmonad="nvim ~/.xmonad/xmonad.hs"
alias nfi3="nvim ~/MEGA/dotfiles/wm/i3/i3config.hs"
# }}}
# fun {{{
alias oi="echo 'oi, tenha um bom dia'"
alias tudoerrado="echo 'não desista as coisas vão dar certo'"
alias culpasua="echo 'não é a maquina que comete erros é o programador'"
alias socorro="echo 'continue a nadar'"
alias resposta="echo '42'"
alias answer="echo '42'"
alias frase="echo 'We live in a world of possibilities'"
alias programingtime="echo 'let´s have some fun'"
# }}}
# usefull {{{
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
alias rm='rm -i'
alias cp='cp -ai'
alias .='pwd'
alias ..='cd ..'
alias ...='cd ../..'
alias grep='grep --color=auto'
alias df='df -h'
alias df10='df -H'
alias du='du -h'
# }}}
# }}}
# Global variables {{{
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=7'
LS_COLORS='di=1;33';export LS_COLORS
# }}}
# Settings on history {{{
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt hist_ignore_all_dups
# }}}
# Hashs {{{
hash -d meg=~/MEGA
hash -d dro=~/Dropbox
hash -d dri=~/MEGA/Drive
hash -d ran=~/random
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
zstyle :compinstall filename '/home/gabriel/.oh-my-zsh/custom/my_config.zsh'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# }}}
# Others {{{
bindkey -v # vim keys

# work nice with emacs
if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
  export ATHAME_ENABLED=0
  bindkey -e
fi

neofetch # neofetch (begin)
# }}}
