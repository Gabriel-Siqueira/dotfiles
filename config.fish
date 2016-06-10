set PATH $PATH /bin

# Prompt
#
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

	set -l up_beg $color3" ┌╼$color1 ⬖ -("
	set -l up_mid $color2(basename $PWD)
	set -l sep $normal$color3"<->"
	set -l up_end $color1")-⬗"$normal $sep $color1(date "+%d/%m/%y")$normal $sep $color1(date "+%T")
    set -l down $normal $color3"└╼" $color1(users)$normal$color3":➤ " $normal

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

function oi
	echo 'oi, tenha um bom dia'
end
function tudoerrado
	echo 'não desista as coisas vão dar certo'
end
function culpasua
	echo 'não é a maquina que comete erros é o programador'
end
function socorro
	echo 'continue a nadar'
end
function resposta
	echo '42'
end
function answer
	echo '42'
end
function frase
	echo 'We live in a world of possibilities'
end
function programingtime
	echo 'let´s have some fun'
end

function clr
	clear
end
function gccs
	gcc -ansi -pedantic -Wall -Werror -lm $argv
end
function l
	ls $argv
end
function ls
	command ls --color=auto $argv
end
function dot
	ls .[a-zA-Z0-9_]* $argv
end
function ll
	ls -l $argv
end
function la
	ls -A $argv
end
function lh
	ls -lh $argv
end
function ltr
	ls -ltr $argv
end
function cd..
	cd ..
end
function mkdir
	command mkdir -p $argv
end
function rm
	command rm -i $argv
end
function mv
	command mv -i $argv
end
function cp
	command cp -ai $argv
end
function .
	pwd $argv
end
function ..
	pwd ..
end
function ...=
	pwd ../..
end
function grep
	command grep --color=auto $argv
end
function df
	command df -h $argv
end
function df10
	command df -H $argv
end
function du
	command du -h $argv
end

#vi keys
fish_vi_key_bindings

# screenfetch (begin)
screenfetch
