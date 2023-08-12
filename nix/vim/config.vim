"{{{ ====================== Functions ======================

function! My_SwitchSpellLang()
	" Loop through languages.
	let &l:spelllang = s:myLangList[s:myLang] | setlocal spell
	echomsg 'language:' s:myLangList[s:myLang]
	let s:myLang = s:myLang + 1
	if s:myLang >= len(s:myLangList) | let s:myLang = 0 | endif
endfunction

function! My_Daily()
	" Open daily routine/activities.
	execute "tabe " . $MY_WIKI . "weekday.md"
	execute "vsplit"
	execute "TW daily"
endfunction

function! My_OpenBib(how)
	" Open file associated with reference under the cursor.
	let save_reg = @@
	let @@ = "failure"
	execute "normal! yi["
	if @@ == "failure"
		execute "normal yi,"
		if @@ == "failure"
			execute "normal! yi{"
			let cite = split(@@,',')[0]
		else
			let cite = @@
		endif
	else
		let cite = split(@@,'@')[0]
	endif
	if a:how == "pdf"
		let file = $MY_REFS . cite . ".pdf"
		echo file
		call system("xdg-open " . file . "&")
	elseif a:how == "bib"
		execute "e $MY_BIB"
		call search(cite)
	else
		echomsg 'Error: Option non existent!'
	endif
	let @@ = save_reg
endfunction

function! My_setNum(num)
	" Set number in number register.
	let @n = a:num
endfunction

function! My_nextNum()
	" Replace character with number of number register and increment number
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

function! My_Tags()
	if &filetype=='haskell'
		call system("fast-tags -R .")
	else
		call system("ctags -R .")
	endif
endfunction

function! My_Index()
	if &filetype=='tex'
		exe 'LatexTOC'
	else
		exe 'Vexplore'
	endif
endfunction

function! My_show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocActionAsync('doHover')
  endif
endfunction

function! My_Pdf()
	let pdf = expand('%:p:r') . '.pdf'
	let pdfs = glob('*.pdf',0,1)
	if filereadable(pdf)
		call system("xdg-open " . pdf . "&")
	elseif !empty(pdfs)
		call system("xdg-open " . pdfs[0] . "&")
	else
		echomsg 'No pdf available'
	endif
endfunction

function! My_SendLine()
    execute "normal \<Plug>(ripple_send_line)"
    normal j
endfunction

" }}}
"{{{ ====================== Mappings =======================

let g:mapleader = "\<Space>"
let g:maplocalleader = '\\'
call which_key#register('<Space>', "g:which_key_map")

let g:which_key_map = {
			\ '<Tab>' : [':b#', 'go to previous buffer'],
			\ }

let g:which_key_map.a = {
			\ 'name' : '+apps',
			\ 'u' : [':GundoToggle', 'undo tree'],
			\ 't' : [':!ctags -R .<cr>', 'generate tags'],
			\ }

let g:which_key_map.a.m = {
			\ 'name' : '+make',
			\ 'm' : [':Make', 'make'],
			\ 'c' : [':Make clean', 'clean'],
			\ }

" let g:which_key_map.a.w = {
"             \ 'name' : '+wiki',
"             \ 'w' : ['<Plug>VimwikiIndex', 'open index file'],
"             \ 'd' : ['<Plug>VimwikiDiaryIndex', 'open diary index file'],
"             \ 't' : ['<Plug>VimwikiTabIndex', 'open index file in new tab'],
"             \ 's' : ['<Plug>VimwikiUISelect', 'select between multiple wikis'],
"             \ 'e' : ['<Plug>VimwikiMakeDiaryNote', 'new diary entry'],
"             \ 'h' : ['<Plug>:VimwikiAll2HTML', 'convert to html'],
            " \ }

let g:which_key_map.a.l = {
	\ 'name' : '+ladger',
	\ 'l': [':e $MY_LEDGER', 'open ledger file'],
	\ 'b': [':Ledger bal', 'balance'],
	\ }

let g:which_key_map.b = {
			\ 'name' : '+buffer',
			\ 'b' : [':Telescope buffers', 'show buffers'],
			\ 'd' : [':bd', 'delete buffer'],
			\ 'n' : [':bn', 'next buffer'],
			\ 'p' : [':bp', 'prev buffer'],
			\ }

let g:which_key_map.e = {
			\ 'name' : '+errors',
			\ 'l' : [':lua vim.diagnostic.show()','list'],
			\ 'n' : [':lua vim.diagnostic.goto_next()','next'],
			\ 'p' : [':lua vim.diagnostic.goto_prev()','prev'],
			\ }

let g:which_key_map.f = {
			\ 'name' : '+files',
			\ 's' : [':update', 'save'],
			\ 'f' : [':Telescope find_files', 'open file'],
			\ 'a' : [':FSHere', 'alternate'],
			\ }

let g:which_key_map.g = {
	\ 'name' : '+git',
	\ 's' : [':Git', 'git-status'],
	\ 'S' : [':Git add %', 'stage-current-file'],
	\ 'U' : [':Git reset -q %', 'unstage-current-file'],
	\ 'c' : [':Git commit', 'edit-git-commit'],
	\ 'p' : [':Git push', 'git-push'],
	\ 'd' : [':Git diff', 'view-git-diff'],
	\ 'A' : [':Git add .', 'stage-all-files'],
	\ 'b' : [':Git blame', 'view-git-blame'],
	\ 'V' : [':Git log -- %', 'git-log-of-current-file'],
	\ 'v' : [':Git log --', 'git-log-of-current-repo'],
	\ }

let g:which_key_map.l = {
			\ 'name' : '+language',
			\ 'r' : [':lua vim.lsp.buf.rename{}', 'rename'],
			\ 'f' : [':lua vim.lsp.buf.format{}', 'format'],
			\ 'a' : [':lua vim.lsp.buf.code_action{}', 'action']
			\ }

let g:which_key_map.l.j = {
			\ 'name' : '+jump',
			\ 'd' : [':lua vim.lsp.buf.definition{}', 'definition'],
			\ 'i' : [':lua vim.lsp.buf.implementation{}', 'implementation'],
			\ 'r' : 'references',
			\ }
noremap <silent> <leader>ljr :lua require'telescope.builtin'.lsp_references{}<CR>
noremap K :lua vim.lsp.buf.hover{}<CR>

noremap <silent> <leader>ns :call My_setNum(v:count1)<CR>
noremap <silent> <leader>nn :call My_nextNum()<CR>
let g:which_key_map.n = {
			\ 'name' : '+numbers',
			\ 's' : 'set',
			\ 'n' : 'next',
			\ }

nnoremap <silent> <leader>od :call My_Daily()<CR>
nnoremap <silent> <leader>op :call My_Pdf()<CR>
nnoremap <silent> <leader>oi :call My_Index()<CR>
let g:which_key_map.o = {
			\ 'name' : '+open',
			\ 'w' : [':silent !firefox %', 'open in firefox'],
			\ 'd' : 'daily view',
			\ 'p' : 'pdf',
			\ 'i' : 'index',
			\ }

nnoremap <silent> <leader>ocp :call My_OpenBib("pdf")<CR>
nnoremap <silent> <leader>occ :call My_OpenBib("bib")<CR>
let g:which_key_map.o.c = {
			\ 'name' : '+citation',
			\ 'p' : 'pdf',
			\ 'c' : 'bib',
			\ }

" let g:which_key_map.r = {
" 	\ 'name' : '+tmuxrunner',
" 	\ 'r' : [':VtrResizeRunner', 'rezise'],
" 	\ 't' : [':VtrReorientRunner', 'reorient'],
" 	\ 's' : [':VtrSendCommandToRunner', 'send command'],
" 	\ 'l' : [':VtrSendLinesToRunner', 'send lines'],
" 	\ 'o' : [':VtrOpenRunner', 'open'],
" 	\ 'k' : [':VtrKillRunner', 'kill'],
" 	\ 'f' : [':VtrFocusRunner', 'focus'],
" 	\ 'd' : [':VtrDetachRunner', 'detach'],
" 	\ 'a' : [':VtrReattachRunner', 'reattach'],
" 	\ 'e' : [':VtrClearRunner', 'clear'],
" 	\ 'c' : [':VtrFlushCommand', 'flush'],
" 	\ }

" let g:which_key_map.r = {
"             \ 'name' : '+repl',
"             \ 'r' : 'send line',
"             \ 's' : ['<Plug>(ripple_send_motion)', 'send motion'],
"             \ 'o' : ['<Plug>(ripple_open_repl)', 'open']
"             \ }
" vmap <leader>rs <Plug>(ripple_send_selection)
" nnoremap <silent> <leader>rr :call My_SendLine()<CR>

nmap <leader>sc :noh<CR>
let g:which_key_map.s = {
			\ 'name' : '+search',
			\ 's' : [':Telescope live_grep', 'live grep'],
			\ 'c' : 'clear',
			\ }

nnoremap <silent> <leader>Sc :call My_SwitchSpellLang()<CR>
let g:which_key_map.S = {
			\ 'name' : '+syntax',
			\ 'c' : 'change dictionary',
			\ 's' : [':setlocal spell!', 'toggle spell check'],
			\ }

let g:which_key_map.S.g = {
	\ 'name' : '+grammarous',
	\ 'i' : ['<Plug>(grammarous-open-info-window)', 'open grammar info window'],
	\ 'g' : [':GrammarousCheck ', 'start grammar checker'],
	\ }

nnoremap <silent> <leader>tg :call My_Tags()<CR>
let g:which_key_map.t = {
			\ 'name' : '+tabs/tags',
			\ 'n' : [':tabnew', 'new tab'],
			\ 'o' : [':tabonly', 'single tab'],
			\ 'c' : [':tabclose', 'close tab'],
			\ 'm' : [':tabmove', 'move tab'],
			\ 't' : [':Tags', 'search tags'],
			\ 'g' : 'generate tags',
			\ }

let g:which_key_map.w = {
			\ 'name' : '+windows',
			\ 'w' : ['<C-W>w', 'other-window'],
			\ 'd' : ['<C-W>c', 'delete-window'],
			\ 'h' : ['<C-W>h', 'window-left'],
			\ 'j' : ['<C-W>j', 'window-below'],
			\ 'l' : ['<C-W>l', 'window-right'],
			\ 'k' : ['<C-W>k', 'window-up'],
			\ 'H' : ['<C-W>H', 'move-window-left'],
			\ 'J' : ['<C-W>J', 'move-window-below'],
			\ 'L' : ['<C-W>L', 'move-window-right'],
			\ 'K' : ['<C-W>K', 'move-window-up'],
			\ '<' : ['<C-W>5<', 'expand-window-left'],
			\ ',' : [':resize +5', 'expand-window-below'],
			\ '>' : ['<C-W>5>', 'expand-window-right'],
			\ '.' : [':resize -5', 'expand-window-up'],
			\ '=' : ['<C-W>=', 'balance-window'],
			\ 's' : ['<C-W>s', 'split-window-below'],
			\ 'v' : ['<C-W>v', 'split-window-right'],
			\ 'z' : [':ZoomWinTabToggle', 'zoom toggle'],
			\ }

let g:which_key_map['<Leader>'] = {
			\ 'name' : '+easymotion',
			\ }
call which_key#register('<Space>', "g:which_key_map")

nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>

nmap <leader><leader>c <Plug>(easymotion-s)
nmap <leader><leader>C <Plug>(easymotion-overwin-f)
nmap <leader><leader>w <Plug>(easymotion-bd-w)
nmap <leader><leader>W <Plug>(easymotion-overwin-w)
nmap <leader><leader>l <Plug>(easymotion-bd-jk)
nmap <leader><leader>L <Plug>(easymotion-overwin-line)
nmap <leader><leader>; <Plug>(easymotion-next)
nmap <leader><leader>, <Plug>(easymotion-prev)
nmap <leader><leader><leader> <Plug>(easymotion-jumptoanywhere)

" snippets
imap <M-/> <Plug>(coc-snippets-expand)
vmap <M-/> <Plug>(coc-snippets-select)
let g:coc_snippet_next = '<c-j>'
let g:coc_snippet_prev = '<c-k>'

" swap visual and real line motions
noremap  k gk
noremap  j gj
noremap  0 g0
noremap  $ g$
noremap  gk k
noremap  gj j
noremap  g0 0
noremap  g$ $

" augroup TaskwarriorMapping
" 	autocmd FileType taskreport nmap <buffer> <CR> :call TaskFile()<CR>
" 	autocmd FileType taskreport nmap <buffer> t :call Tomorrow()<CR>
" 	autocmd FileType taskreport nmap <buffer> u :call Undo()<CR>
" augroup END

" accept habits
cnoreabbrev Q q
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Wq q
cnoreabbrev X x
cnoreabbrev xx X
nnoremap Y y$

" Mouse bindings
" Disable middle mouse button paste
:map <MiddleMouse> <Nop>
:imap <MiddleMouse> <Nop>
:map <2-MiddleMouse> <Nop>
:map! <2-MiddleMouse> <Nop>
:map <3-MiddleMouse> <Nop>
:map! <3-MiddleMouse> <Nop>
:map <4-MiddleMouse> <Nop>
:map! <4-MiddleMouse> <Nop>
"}}}
"{{{ ====================== Settings =======================

" misc {{{

filetype plugin indent on " detection of filetype, plugin and indentation
syntax on

set backspace=indent,eol,start " make backspace work
set nocompatible     " no need to be compatible with vi
set shiftwidth=4     " size off >>, << and ==
set tabstop=4        " size of a <tab>
set expandtab        " use spaces instead of tab
set autoindent       " automatic insert indentation
set number           " Line number
set numberwidth=2    " size of numbers
set cursorline       " Highlight cursor line
set textwidth=0      " Do not break lines
if has('linebreak')
	set wrap             " make virtual lines
	set linebreak        " wrap long lines at characters in 'breakat'
	let &showbreak='⤷ '  " character for line break (U+2937, UTF-8: E2 A4 B7)
endif
set mouse=a          " enable mouse
set mousemodel=popup " right button open pupup menu
set scrolloff=10     " Keep cursor centered
set sidescrolloff=3  " scrolloff for columns
set showmatch        " Show matching brackets when text indicator is over them
set hidden           " Do not need to save buffers to open others
if has('termguicolors')
	set termguicolors
endif
set infercase        " when using upper case complete only with upper case
set encoding=utf8    " Set utf8 as standard encoding
if exists('&inccommand')
	set inccommand=split " split in incremental search
endif
set redrawtime=500   " time limit to highlight search
set autoread         " Set to auto read when a file is changed from the outside
set splitright       " split on the right
set noshowmode        " remove text insert from command line (status line already have it)
set virtualedit=block " better block selection
set fileformats=unix,dos,mac " order of file format
set laststatus=3      " Always display the status line
set lazyredraw        " Don't redraw while executing macros (good performance)
set list listchars=tab:»\ ,trail:-,extends:>,precedes:<,eol:¬,nbsp:·
set omnifunc=syntaxcomplete#Complete " omnicompletion
set showcmd           " Show current commands
if has('wildmenu')
	set wildmenu        " show options as list when switching buffers etc
endif
set wildmode=longest:full,full " shell-like autocomplete to unambiguous portion
set path+=**           " look for files in all levels of subdirectories
if has('windows')
	set fillchars=vert:┃
endif
set cmdheight=2        " give more space for displaying messages
set shortmess+=c       " don't pass messages to ins-completion-menu
set foldmethod=expr    " fold using expressions
set signcolumn=yes     " display signs with the numbers
set spell              " spell check on by defaut

" List of languages to toggle between
let s:myLang = 0
let s:myLangList = ['pt_br', 'en_us', 'fr']

" Color
" let s:myBackground=readfile("/tmp/theme.txt")
" echomsg s:myBackground[0]
" if s:myBackground[0]=='light'
"     set background=light
" else
"     set background=dark
" endif
set background=dark
try
	colorscheme gruvbox
catch /^Vim\%((\a\+)\)\=:E185/
endtry


" set backup           " keep backup file after write
set undofile         " use undo files

" don't create root-owned files
if exists('$SUDO_USER')
	if has('persistent_undo')
		set noundofile
	endif
	set noswapfile
	set nobackup
	set nowritebackup
endif

" }}}
" languages/file types {{{
" haskell {{{

au! BufEnter *.hs let b:fswitchdst = 'hs' | let b:fswitchlocs = '../test' | let b:fswitchfnames = '/$/Check/'
au! BufEnter *Check.hs let b:fswitchdst = 'hs' | let b:fswitchlocs = '../src,../app' | let b:fswitchfnames = '/Check$//'

" }}}
" latex {{{

let g:tex_flavor = 'latex'
let g:vimtex_view_method = 'zathura'
let g:vimtex_grammar_vlty = {'lt_command': 'languagetool'}

" }}}
" nix {{{

au! BufEnter *.nix set commentstring=#%s

" }}}
" }}}
" vim-tmux-runner {{{

let g:VtrStripLeadingWhitespace = 0
let g:VtrClearEmptyLines = 0
let g:VtrAppendNewline = 1
let g:VtrOrientation = "h"
let g:VtrPercentage = 50

" }}}
" ripple {{{

let g:ripple_repls = {}
let g:ripple_repls["haskell"] = {
    \ "command": "stack ghci",
    \ "pre": ":{",
    \ "post": ":}",
    \ "addcr": 0,
    \ "filter": 0,
    \ }

" }}}
" vim: foldmethod=marker foldlevel=0
