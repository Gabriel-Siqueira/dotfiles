set PATH $PATH /bin

# Prompt
if ls bin | grep powerline-shell
    function fish_prompt
        ~/bin/powerline-shell/powerline-shell.py $status --shell bare ^/dev/null
    end
else
    function _git_branch_name
      echo (git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||')
    end

    function _is_git_dirty
      echo (git status -s --ignore-submodules=dirty ^/dev/null)
    end

    function fish_prompt
        set -l color1 (set_color -o cyan)
        set -l color2 (set_color -o green)
        set -l color3 (set_color -o white)
        set -l color4 (set_color -o red)
        set -l normal (set_color normal)

        set -l up_beg $color3"┌╼$color1 ⬖ -("
        set -l up_mid $color2(basename $PWD)
        set -l sep $normal$color3"<->"
        set -l up_end $color1")-⬗"$normal $sep $color1(date "+%d/%m/%y")$normal $sep $color1(date "+%T")
        set -l down $normal $color3"   └╼" $color1(users | cut -d ' ' -f 1)$normal$color3":➤ " $normal


      if [ (_git_branch_name) ]
          set -l git_branch $color2(_git_branch_name)
          set git_info "$sep$color1 git:($git_branch$normal$color1)$normal"

          if [ (_is_git_dirty) ]
            set -l dirty "$color4 ✗"
            set git_info "$git_info$dirty"
          else
            set -l dirty "$color2 ✔"
            set git_info "$git_info$dirty"
          end
        end

        echo $up_beg $up_mid $up_end $git_info
        echo $down
    end
end

# Functions

# Manda para lixeira
function trash
	mv $argv ~/.local/share/Trash/files/;
end

# Use ssh on ic
function ic_term
	ssh ra155446@ssh.students.ic.unicamp.br
end

# aleases

alias oi "echo 'oi, tenha um bom dia'"
alias tudoerrado "echo 'não desista as coisas vão dar certo'"
alias culpasua "echo 'não é a maquina que comete erros é o programador'"
alias socorro "echo 'continue a nadar'"
alias resposta "echo '42'"
alias clr "clear"
alias gccs "gcc -ansi -pedantic -Wall -Werror -lm $argv"
alias l "ls $argv"
alias ls "command ls --color=auto $argv"
alias dot "ls .[a-zA-Z0-9_]* $argv"
alias ll "ls -l $argv"
alias la "ls -A $argv"
alias lh "ls -lh $argv"
alias ltr "ls -ltr $argv"
alias cd.. "cd .."
alias mkdir "command mkdir -p $argv"
alias rm "command rm -i $argv"
alias mv "command mv -i $argv"
alias cp "command cp -ai $argv"
alias . "pwd $argv"
alias .. "pwd .."
alias ...= "pwd ../.."
alias grep "command grep --color=auto $argv"
alias df "command df -h $argv"
alias df10 "command df -H $argv"
alias du "command du -h $argv"

# to work nice on emacs
function fish_title
  true
end

#vi keys
fish_vi_key_bindings

# screenfetch (begin)
screenfetch
