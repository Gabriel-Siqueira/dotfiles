if [ ${EUID} != 0 ] ; then
	local color1="yellow"
	local color2="green"
else
	local color1="red"
	local color2="white"
fi
local color3="gray"

PROMPT='%F{$color3}┌╼%f%F{$color1} ⬖ -(%f%F{$color2}%~%f%F{$color1})-⬗ %F{$color3}╾╼%f%F{$color1} %D{%d/%m/%y} %f%F{$color3}╾╼ %f%F{$color1}%T%f%F{$color3} $(git_prompt_info)
└╼ %f%F{$color1}%n%f%{$fg_bold[$color3]%}$( vi_mode_prompt_info ):%(!.#.➤) $reset_color'

# RPROMPT="%F{$color1}[%f%F{$color2}%M %f%F{$color1}| %f%F{$color2}%l %f%F{$color1}| %f%F{$color2}%!%f%F{$color1}]"

ZSH_THEME_GIT_PROMPT_PREFIX="╾╼ %{$fg[$color1]%}git:(%{$fg[$color2]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[$color1]%}) %{$fg[red]%}✗"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[$color1]%}) %{$fg[yellow]%}?"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[$color1]%}) %{$fg[green]%}✔"

MODE_INDICATOR="%{$fg_bold[red]%}"
