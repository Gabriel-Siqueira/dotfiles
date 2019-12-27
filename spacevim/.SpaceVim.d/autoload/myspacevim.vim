function! myspacevim#before() abort
  let g:myLang = 0
  let g:myLangList = ['pt', 'en']
endfunction

function! myspacevim#after() abort
  " Settings
  set background=dark
  set foldmethod=expr
  set wrap
  set breakindent
  set inccommand=nosplit
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
  " Commands
  command ClearPlugins :call ClearPlugins()
endfunction

function! SwitchSpellLang()
  " Loop through languages.
  let &l:spelllang = g:myLangList[g:myLang] | setlocal spell
  echomsg 'language:' g:myLangList[g:myLang]
  let g:myLang = g:myLang + 1
  if g:myLang >= len(g:myLangList) | let g:myLang = 0 | endif
endfunction

function! ClearPlugins()
  " Clear unused plugins.
	call map(dein#check_clean(), "delete(v:val, 'rf')")
	call dein#recache_runtimepath()
endfunction
