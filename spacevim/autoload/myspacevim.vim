" Reserved registers
" n - number register

function! myspacevim#before() abort
  let s:myLang = 0
  let s:myLangList = ['pt', 'en']
  " Which Key
  call SpaceVim#custom#SPCGroupName(['o'], '+Open')
  call SpaceVim#custom#SPC('nmap', ['o','c'], 'call My_OpenBib()', 'citation under cursor', 1)
  call SpaceVim#custom#SPC('nmap', ['o','w'], 'silent !chromium %', 'open in chromium', 1)
  call SpaceVim#custom#SPC('nmap', ['o','d'], 'call My_Daily()', 'daily view', 1)
  call SpaceVim#custom#SPCGroupName(['S'], '+Syntax')
  call SpaceVim#custom#SPC('nmap', ['S','c'], 'call My_SwitchSpellLang()', 'change dictionary', 1)
  call SpaceVim#custom#SPC('nmap', ['S','s'], 'setlocal spell!', 'toggle spell check', 1)
  call SpaceVim#custom#SPCGroupName(['a','m'], '+Make')
  call SpaceVim#custom#SPC('nmap', ['a','m','m'], 'make -C %:p:h', 'run make on file path', 1)
  call SpaceVim#custom#SPC('nmap', ['a','m','c'], 'make clean -C %:p:h', 'run make clean on file path', 1)
  call SpaceVim#custom#SPC('nmap', ['n','s'], '<C-U> call My_setNum(v:count1)', 'set', 1)
  call SpaceVim#custom#SPC('nmap', ['n','n'], 'call My_nextNum()', 'next', 1)
endfunction

function! myspacevim#after() abort
  " Settings
  set background=dark
  set foldmethod=expr
  set wrap
  set breakindent
  set inccommand=split
  set redrawtime=500
	set expandtab
  " Mappings
  nnoremap Y y$
  augroup TaskwarriorMapping
    autocmd FileType taskreport nmap <buffer> <CR> :call TaskFile()<CR>
  augroup END
  imap <C-k> <Plug>(neosnippet_jump)
  smap <C-k> <Plug>(neosnippet_jump)
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

function! My_SwitchSpellLang()
  " Loop through languages.
  let &l:spelllang = s:myLangList[s:myLang] | setlocal spell
  echomsg 'language:' s:myLangList[s:myLang]
  let s:myLang = s:myLang + 1
  if s:myLang >= len(s:myLangList) | let s:myLang = 0 | endif
endfunction

function! ClearPlugins()
  " Clear unused plugins.
	call map(dein#check_clean(), "delete(v:val, 'rf')")
	call dein#recache_runtimepath()
endfunction

function! My_Daily()
	" Opend daily rotine/activities.
	execute "tabe " . $MY_WIKI . "weekday.md"
	execute "vsplit"
	execute "TW daily"
endfunction

function! My_OpenBib()
  " Opend file associated with reference under the cursor.
  let save_reg = @@
  execute "normal! yi["
  let file = $MY_REFS . split(@@,'@')[0] . ".pdf"
  echo file
  call system("xdg-open " . file . "&")
  let @@ = save_reg
endfunction

function! My_setNum(num)
	" Set number in numer register.
	let @n = a:num
endfunction

function! My_nextNum()
	" Replace caracter with number of number register and incremente number
	" register.
	execute "normal a\<BS>\<ESC>\"np"
	let num = @n
	let @n = num + 1
endfunction

function! VimwikiLinkHandler(link)
  " Use Vim to open external files with the 'vfile:' scheme.  E.g.:
  "   1) [[vfile:~/Code/PythonProject/abc123.py]]
  "   2) [[vfile:./|Wiki Home]]
  let link = a:link
  if link =~# '^vfile:'
    let link = link[1:]
  else
    return 0
  endif
  let link_infos = vimwiki#base#resolve_link(link)
  if link_infos.filename == ''
    echomsg 'Vimwiki Error: Unable to resolve link!'
    return 0
  else
    exe 'tabnew ' . fnameescape(link_infos.filename)
    return 1
  endif
endfunction
