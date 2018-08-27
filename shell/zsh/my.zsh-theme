if [ ${EUID} != 0 ] ; then
    local color1="yellow"
    local color2="green"
else
    local color1="red"
    local color2="white"
fi
local color3="white"

MODE_INDICATOR="%{$fg_bold[red]%}"

local theme_precmd () {
    # Git number of changes
    if git rev-parse --is-inside-work-tree 2> /dev/null | grep -q 'true' ; then
        local git_status=$(git status --short | wc -l | awk '{$1=$1};1')
    fi
    # Check if has sudo
    local with_sudo=$(sudo -n uptime 2>&1|grep "load"|wc -l)
    if [ ${with_sudo} -gt 0 ]; then
        local sudo=",%{$fg[red]%}#%{$reset_color%}"
    else
        local sudo=""
    fi
    # Check if is in a virtualenv
    local in_venv=$(echo $VIRTUAL_ENV)
    if [ "${in_venv}" = "" ]; then
        local venv=""
    else
        local venv=",%{$fg[magenta]%}$(basename $VIRTUAL_ENV)%{$reset_color%}"
    fi

    # Begin
    local begin="%F{$color3}┌╼%f"

    # Host name
    local host="%F{$color1} (%f%F{$color2}%m%f%F{$color1}) %f%F{$color3}╾╼%f"

    # Path
    local path="%F{$color1} (%f%F{$color2}%~%f%F{$color1}) %f%F{$color3}╾╼%f"

    # Date time
    local date="%F{$color1} (%f%F{$color2}%T%f%F{$color1}) %f%F{$color3}╾╼%f"

    # GIT
    local git='$(git_prompt_info)'

    ZSH_THEME_GIT_PROMPT_PREFIX="
%F{$color3}┝ %f%F{$color1}git(%F{gray}$git_status%f:%F{$color2}"
    ZSH_THEME_GIT_PROMPT_SUFFIX="%F{$color1})"
    ZSH_THEME_GIT_PROMPT_SEPARATOR="|"
    ZSH_THEME_GIT_PROMPT_BRANCH=" %{$fg[magenta]%}"
    ZSH_THEME_GIT_PROMPT_STAGED=" %{$fg[red]%}%{●%G%}"
    ZSH_THEME_GIT_PROMPT_CONFLICTS=" %{$fg[red]%}%{✖%G%}"
    ZSH_THEME_GIT_PROMPT_CHANGED=" %{$fg[blue]%}%{✚%G%}"
    ZSH_THEME_GIT_PROMPT_BEHIND=" %{↓%G%}"
    ZSH_THEME_GIT_PROMPT_AHEAD=" %{↑%G%}"
    ZSH_THEME_GIT_PROMPT_UNTRACKED=" %{…%G%}"
    ZSH_THEME_GIT_PROMPT_CLEAN=" %{$fg[green]%}%{✔%G%}%f"
    ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}✗"

    # SVN
    local svn='$(svn_prompt_info)'

    ZSH_THEME_SVN_PROMPT_PREFIX="
%F{$color3}┝ %f%F{$color1}svn(%F{$color2}"
    ZSH_THEME_SVN_PROMPT_SUFFIX="%F{$color1})"
    ZSH_THEME_SVN_PROMPT_CLEAN=" %{$fg[green]%}%{✔%G%}%f"
    ZSH_THEME_SVN_PROMPT_DIRTY=" %{$fg[red]%}✗"

    # Other info

    local other="%F{$color1} [%f"

    # Number of jobs
    other+="%{$fg[blue]%}%j%{$reset_color%}"
    # Virtualenv
    other+="$venv"
    # Status code
    other+="%(?..,%{$fg[red]%}%?%{$reset_color%})"
    # Sudo
    other+="$sudo"

    other+="%F{$color1}]%f"

	# New line
	local new_line="
%F{$color3}└╼ %f"

    # name
    local name="%F{$color1}%n%f"

    # End
    local end='%{$fg_bold[$color3]%}$( vi_mode_prompt_info )%(!.#.$) $reset_color'

	# PROMPT
	local zero='%([BSUbfksu]|([FK]|){*})' # patter to be removed when calculating size
    local top="$begin$host$path$date$other"
	local size=${#${(S%%)top//$~zero/}} 
    if [ $size -gt $COLUMNS ]; then
		local top="$begin$path$date$other"
		local size=${#${(S%%)top//$~zero/}} 
		if [ $size -gt $COLUMNS ]; then
			local top="$begin$date$other"
			local size=${#${(S%%)top//$~zero/}} 
			if [ $size -gt $COLUMNS ]; then
				local top="$begin$other"
				local size=${#${(S%%)top//$~zero/}} 
				if [ $size -gt $COLUMNS ]; then
					local top=""
					local new_line=""
					local git=""
					local svn=""
				fi
			fi
		fi
	fi
    bottom="$new_line$name$end"
	local size=${#${(S%%)bottom//$~zero/}} 
	if [ $size -gt $COLUMNS ]; then
		local bottom="$new_line$end"
		local git=""
		local svn=""
	fi
	PROMPT="$top$git$svn$bottom"
}

autoload -U add-zsh-hook
add-zsh-hook precmd theme_precmd
