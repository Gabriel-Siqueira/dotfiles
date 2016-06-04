function! VisualSelection(direction) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gv'
        call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.')
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

function! <SID>SwitchColorSchemes()
	if g:colors_name == 'molokai'
		colorscheme monokay
	elseif g:colors_name == 'molokai'
		colorscheme monokay
	elseif g:colors_name == 'native'
		colorscheme automation
	elseif g:colors_name == 'automation'
	 	colorscheme moria
	elseif g:colors_name == 'moria'
	 	colorscheme desert
	elseif g:colors_name == 'desert'
	 	colorscheme colorful
	elseif g:colors_name == 'colorful'
	 	colorscheme navajo-night
	elseif g:colors_name == 'navajo-night'
	 	colorscheme bmichaelsen
	elseif g:colors_name == 'bmichaelsen'
	 	colorscheme impact
	elseif g:colors_name == 'impact'
	 	colorscheme ir_black
	elseif g:colors_name == 'ir_black'
		colorscheme native
	endif
endfunction
