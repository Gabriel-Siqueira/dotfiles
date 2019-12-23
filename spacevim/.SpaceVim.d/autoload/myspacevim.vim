function! myspacevim#before() abort
  let g:myLang = 0
  let g:myLangList = ['pt', 'en']
endfunction

function! myspacevim#after() abort
	set guicursor+=a:blinkon0
  " Mappings
  nmap <leader>sc :call SwitchSpellLang()<CR>
  nnoremap Y y$
  " Abbrev
  cnoreabbrev Q q
  cnoreabbrev W w
  cnoreabbrev WQ wq
  cnoreabbrev Wq q
  cnoreabbrev X x
  cnoreabbrev xx X
endfunction

function! SwitchSpellLang()
  "loop through languages
  let &l:spelllang = g:myLangList[g:myLang] | setlocal spell
  echomsg 'language:' g:myLangList[g:myLang]
  let g:myLang = g:myLang + 1
  if g:myLang >= len(g:myLangList) | let g:myLang = 0 | endif
endfunction
