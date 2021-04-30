"{{{ ===================== Plugging ========================

if has('nvim')
	call plug#begin('~/.local/share/nvim/plugged')
else
	call plug#begin('~/.vim/plugged')
endif

" Shipped with vim {{{

" Already build in neovim
if !has('nvim')
	packadd! matchit
endif

" }}}
" Misc {{{

Plug 'kana/vim-niceblock'                     " I and A in all visual and better transition of visual
Plug 'nelstrom/vim-visual-star-search'        " * and # search for visual blocks
Plug 'tpope/vim-repeat'                       " extend use of .
Plug 'liuchengxu/vim-which-key'               " show keys
Plug 'junegunn/fzf.vim'                       " use fzf in vim

" }}}
" Objects/Motions {{{

Plug 'kana/vim-textobj-user'                  " help define user objects

Plug 'bkad/CamelCaseMotion'                   " CamelCase word
Plug 'kana/vim-smartword'                     " smart word
Plug 'kana/vim-textobj-entire'                " the hole file
Plug 'kana/vim-textobj-line'                  " line
Plug 'lucapette/vim-textobj-underscore'       " underscore (_)
Plug 'sgur/vim-textobj-parameter'             " parameter of a function
Plug 'thinca/vim-textobj-between'             " between characters
Plug 'tpope/vim-surround'                     " surround
Plug 'easymotion/vim-easymotion'              " move following letters
Plug 'jeetsukumaran/vim-indentwise'           " indentation level motion

" }}}
" Operators {{{

Plug 'kana/vim-operator-user'                 " help define user operators

Plug 'AndrewRadev/splitjoin.vim'              " split and join expressions
Plug 'christoomey/vim-sort-motion'            " sort
Plug 'tpope/tpope-vim-abolish'                " Abbreviate, Substitute and Coerce
Plug 'tpope/vim-commentary'                   " comment in and out
Plug 'tpope/vim-speeddating'                  " fast way of change date
Plug 'tpope/vim-unimpaired'                   " complementary pairs of mappings
Plug 'vim-scripts/ReplaceWithRegister'        " one command to d them p
Plug 'tommcdo/vim-exchange'                   " easily exchange text

" }}}
" Motions {{{


" }}}
" Languages/File types {{{

if has('nvim-0.5')
	Plug 'nvim-treesitter/nvim-treesitter'          " parser for multiple languages
endif
Plug 'sheerun/vim-polyglot'                     " multiple languages
Plug 'neoclide/coc.nvim', {'branch': 'release'} " use language server
Plug 'jceb/vim-orgmode'                         " for org
Plug 'blindFS/vim-taskwarrior'                  " for taskwarrior
Plug 'vimwiki/vimwiki'                          " for vimwiki and markdown
Plug 'ledger/vim-ledger'                        " for ladger
Plug 'Twinside/vim-haskellFold'                 " fold for haskell
Plug 'lervag/vimtex'                            " for latex

" }}}
" Editing/Syntax {{{

Plug 'godlygeek/tabular'                      " align text (:Tab[ularize])
Plug 'jiangmiao/auto-pairs'                   " add pairs automatically
Plug 'rhysd/vim-grammarous'                   " grammar checking
Plug 'honza/vim-snippets'                     " other snippets
" Plug 'w0rp/ale'                               " use linter

" }}}
" Interface {{{

if !has('nvim')
	Plug 'vim-utils/vim-man'                      " view man pages
	Plug 'osyo-manga/vim-over'                    " highlight substitute pattern
	Plug 'Konfekt/FastFold'                       " fold/unfold on right times
endif

Plug 'Shougo/denite.nvim'                     " search/display info (file, buf)
Plug 'juanpabloaj/help.vim'                   " better navigation in help
Plug 'seagoj/last-position.vim'               " save cursor position
Plug 'sjl/gundo.vim'                          " undo tree
Plug 'troydm/zoomwintab.vim'                  " make pane full screen
Plug 'vim-airline/vim-airline'                " new mode line
Plug 'vim-airline/vim-airline-themes'         " themes for airline
Plug 'tpope/vim-dispatch'                     " asynchrous make
Plug 'derekwyatt/vim-fswitch'                 " alternate files

" }}}
" Applications interface {{{

" Plug 'blindFS/vim-taskwarrior'           " work with taskwarrior
Plug 'tpope/vim-fugitive'                " work with git
Plug 'szw/vim-tags'                      " automatic generate tags on save
Plug 'tpope/vim-eunuch'                  " unix commands inside vim
Plug 'knubie/vim-kitty-navigator'        " same navigation vim and kitty

" Tmux
" Plug 'christoomey/vim-tmux-navigator'    " same navigation vim and tmux
Plug 'christoomey/vim-tmux-runner'       " send commands/code from vim to tmux
Plug 'roxma/vim-tmux-clipboard'          " same clipboard vim and tmux

" }}}
""{{{ Colors

Plug 'lifepillar/vim-solarized8'
Plug 'tomasr/molokai'
Plug 'morhetz/gruvbox'
Plug 'NLKNguyen/papercolor-theme'

""}}}

call plug#end()

"}}}
"{{{ ====================== Commands =======================

command! -nargs=0 Clean   :set nonu norelativenumber nolist foldcolumn=0
command! -nargs=0 Unclean :set nu relativenumber list foldcolumn=1
command! -nargs=0 Envim   :e ~/.config/nvim/init.vim
command! -nargs=0 Evim    :e ~/.vimrc
command! -nargs=0 So      :so $MYVIMRC | set foldmethod=marker

"}}}
"{{{ ====================== Functions ======================

function! My_SwitchSpellLang()
	" Loop through languages.
	let &l:spelllang = s:myLangList[s:myLang] | setlocal spell
	echomsg 'language:' s:myLangList[s:myLang]
	let s:myLang = s:myLang + 1
	if s:myLang >= len(s:myLangList) | let s:myLang = 0 | endif
endfunction

function! My_Daily()
	" Opend daily rotine/activities.
	execute "tabe " . $MY_WIKI . "weekday.md"
	execute "vsplit"
	execute "TW daily"
endfunction

function! My_OpenBib(how)
	" Opend file associated with reference under the cursor.
	let save_reg = @@
	let @@ = "failure"
	execute "normal! yi["
	if @@ == "failure"
		execute "normal! yi{"
		let cite = split(@@,',')[0]
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

let g:which_key_map.a.w = {
			\ 'name' : '+wiki',
			\ 'w' : ['<Plug>VimwikiIndex', 'open index file'],
			\ 'd' : ['<Plug>VimwikiDiaryIndex', 'open diary index file'],
			\ 't' : ['<Plug>VimwikiTabIndex', 'open index file in new tab'],
			\ 's' : ['<Plug>VimwikiUISelect', 'select between multiple wikis'],
			\ 'e' : ['<Plug>VimwikiMakeDiaryNote', 'new diary entry'],
			\ 'h' : ['<Plug>:VimwikiAll2HTML', 'convert to html'],
			\ }

let g:which_key_map.a.l = {
			\ 'name' : '+ladger',
			\ 'l': [':e $MY_LEDGER', 'open ledger file'],
			\ 'b': [':Ledger bal', 'balance'],
			\ }

let g:which_key_map.a.t = {
			\ 'name' : '+task',
			\ 'p' : [':TW planing', 'open taskwarrior planing report'],
			\ 't' : [':TW tomorrow', 'open taskwarrior report for tommorrow'],
			\ 'd' : [':TW daily', 'open taskwarrior report for today'],
			\ }

let g:which_key_map.b = {
			\ 'name' : '+buffer',
			\ 'b' : [':Buffers', 'show buffers'],
			\ 'd' : [':bd', 'delete buffer'],
			\ 'n' : [':bn', 'next buffer'],
			\ 'p' : [':bp', 'prev buffer'],
			\ }

let g:which_key_map.e = {
			\ 'name' : '+errors',
			\ 'l' : [':CocDiagnostics','list'],
			\ 'n' : ['<Plug>(coc-diagnostic-next)','next'],
			\ 'p' : ['<Plug>(coc-diagnostic-prev)','prev'],
			\ }

let g:which_key_map.f = {
			\ 'name' : '+files',
			\ 's' : [':update', 'save'],
			\ 'f' : [':Files', 'open file'],
			\ 'a' : [':FSHere', 'alternate'],
			\ }

let g:which_key_map.g = {
			\ 'name' : '+git',
			\ 's' : ['Gstatus', 'git-status'],
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

noremap <silent> <leader>ld :call My_show_documentation()<CR>
let g:which_key_map.l = {
			\ 'name' : '+language',
			\ 'd' : 'documentation',
			\ 'r' : ['<Plug>(coc-rename)', 'rename'],
			\ 'f' : ['<Plug>(coc-format)', 'format'],
			\ 'a' : ['<Plug>(coc-action)', 'action']
			\ }

let g:which_key_map.l.j = {
			\ 'name' : '+jump',
			\ 'd' : ['<Plug>(coc-definition)', 'definition'],
			\ 't' : ['<Plug>(coc-type-definition)', 'type definition'],
			\ 'i' : ['<Plug>(coc-implementation)', 'implementation'],
			\ 'r' : ['<Plug>(coc-references)', 'references'],
			\ }

noremap <silent> <leader>ns :call My_setNum(v:count1)<CR>
noremap <silent> <leader>nn :call My_nextNum()<CR>
let g:which_key_map.n = {
			\ 'name' : '+numbers',
			\ 's' : 'set',
			\ 'n' : 'next',
			\ }

nnoremap <silent> <leader>od :call My_Daily()<CR>
nnoremap <silent> <leader>ot :call TaskFile()<CR>
nnoremap <silent> <leader>op :call My_Pdf()<CR>
nnoremap <silent> <leader>oi :call My_Index()<CR>
let g:which_key_map.o = {
			\ 'name' : '+open',
			\ 'w' : [':silent !firefox %', 'open in firefox'],
			\ 'd' : 'daily view',
			\ 't' : 'task file',
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

let g:which_key_map.r = {
			\ 'name' : '+tmuxrunner',
			\ 'r' : [':VtrResizeRunner', 'rezise'],
			\ 't' : [':VtrReorientRunner', 'reorient'],
			\ 's' : [':VtrSendCommandToRunner', 'send command'],
			\ 'l' : [':VtrSendLinesToRunner', 'send lines'],
			\ 'o' : [':VtrOpenRunner', 'open'],
			\ 'k' : [':VtrKillRunner', 'kill'],
			\ 'f' : [':VtrFocusRunner', 'focus'],
			\ 'd' : [':VtrDetachRunner', 'detach'],
			\ 'a' : [':VtrReattachRunner', 'reattach'],
			\ 'e' : [':VtrClearRunner', 'clear'],
			\ 'c' : [':VtrFlushCommand', 'flush'],
			\ }

nmap <leader>sc :noh<CR>
let g:which_key_map.s = {
			\ 'name' : '+search',
			\ 'a' : [':Ag', 'ag'],
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
			\ 'i' : ['<Plug>(grammarous-open-info-window)', 'open info window'],
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
			\ 'H' : ['<C-W>5<', 'expand-window-left'],
			\ 'J' : [':resize +5', 'expand-window-below'],
			\ 'L' : ['<C-W>5>', 'expand-window-right'],
			\ 'K' : [':resize -5', 'expand-window-up'],
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

" other operators
nmap gG <Plug>(operator-grammarous)

" swap visual and real line motions
noremap  k gk
noremap  j gj
noremap  0 g0
noremap  $ g$
noremap  gk k
noremap  gj j
noremap  g0 0
noremap  g$ $

" replace normal words with smart words
map w  <Plug>(smartword-w)
map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
map ge <Plug>(smartword-ge)

call camelcasemotion#CreateMotionMappings('<localleader>')

augroup TaskwarriorMapping
	autocmd FileType taskreport nmap <buffer> <CR> :call TaskFile()<CR>
	autocmd FileType taskreport nmap <buffer> t :call Tomorrow()<CR>
	autocmd FileType taskreport nmap <buffer> u :call Undo()<CR>
augroup END

" accept habits
cnoreabbrev Q q
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Wq q
cnoreabbrev X x
cnoreabbrev xx X
nnoremap Y y$

"}}}
"{{{ ====================== Settings =======================

" misc {{{

filetype plugin indent on " detection of filetype, plugin and indentation
syntax on

set backspace=indent,eol,start " make backspace work
set nocompatible     " no need to be compatible with vi
set shiftwidth=4     " size off >>, << and ==
set tabstop=4        " size of a <tab>
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
set laststatus=2      " Always display the status line
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
set signcolumn=yes  " display signs with the numbers

" List of languages to toggle between
let s:myLang = 0
let s:myLangList = ['pt', 'en']

" Color
set background=dark
try
	colorscheme gruvbox
catch /^Vim\%((\a\+)\)\=:E185/
endtry


set backup           " keep backup file after write
" Where save backup
set backupdir=~/.cache/vim/backup//
set backupdir+=./vim_files//
set backupdir+=~/Documents/vim_files/backup//
set backupdir+=~/Documents/vim_files//
set backupdir+=~/random/vim_files/backup//
set backupdir+=~/random/vim_files//
set backupdir+=~/random//
set backupdir+=.

" Where place swap files
set directory=~/.cache/vim/swap//
set directory+=./vim_files/swap//
set directory+=./vim_files//
set directory+=~/Documents/vim_files/swap//
set directory+=~/Documents/vim_files//
set directory+=~/random/vim_files/swap//
set directory+=~/random/vim_files//
set directory+=~/random//
set directory+=.

" Place for undo files
if has('persistent_undo')
	set undodir=~/.cache/vim/undo//
	set undodir+=./vim_files/undo//
	set undodir+=./vim_files//
	set undodir+=~/Documents/vim_files/undo//
	set undodir+=~/Documents/vim_files//
	set undodir+=~/random/vim_files/undo//
	set undodir+=~/random/vim_files//
	set undodir+=~/random//
	set undodir+=.
	set undofile                      " use undo files
endif

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
" vimwiki {{{

let g:vimwiki_key_mappings = { 'global': 0, }
let g:vimwiki_folding = 'expr'
let g:vimwiki_url_maxsave = 0
let g:vimwiki_markdown_link_ext = 1
let wiki = {}
let wiki.path = '$MY_WIKI'
let wiki.syntax = 'markdown'
let wiki.ext = '.md'
let wiki.auto_diary_index = 1
let wiki.diary_rel_path = 'diary/'
let wiki.nested_syntaxes = {'python': 'python', 'c++': 'cpp', 'haskell': 'haskell', 'zsh': 'zsh'}
let g:vimwiki_list = [wiki]
let g:vimwiki_ext2syntax = {'.md': 'markdown',
			\ '.mkd': 'markdown',
			\ '.wiki': 'markdown'}

" }}}
" taskwarrior {{{

let g:task_default_prompt = ['description', 'due', 'scheduled', 'wait', 'priority', 'project']
let g:task_report_command  = ['uncomplete','daily','planing', 'tomorrow']
let g:task_report_name = 'planing'
let g:task_rc_override = 'defaultwidth=0'

" }}}
" latex {{{

let g:tex_flavor = 'latex'
let g:vimtex_view_method = 'zathura'
let g:vimtex_grammar_vlty = {'lt_command': 'languagetool'}

" }}}
" }}}
" airline {{{

let g:airline_powerline_fonts = 1
let g:airline_theme='murmur'
let g:airline#extensions#capslock#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#wordcount#enabled = 1
let g:airline#extensions#tmuxline#enabled = 0
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 1
let g:airline#extensions#vimtex#enabled = 1
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#coc#enabled = 1
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = '|'

" }}}
" vim-tmux-runner {{{

let g:VtrStripLeadingWhitespace = 0
let g:VtrClearEmptyLines = 0
let g:VtrAppendNewline = 1
let g:VtrOrientation = "h"
let g:VtrPercentage = 50

" }}}
" grammarous {{{

let g:grammarous#disabled_rules = {
			\ '*' : ['REPEATED_WORDS','REPEATED_WORDS_3X', 'BARBARISMS'],
			\ }

" }}}
" netrw {{{

let g:netrw_liststyle = 3
let g:netrw_banner = 0
let g:netrw_browse_split = 4
let g:netrw_winsize = 20

" }}}
" treesitter {{{
"
if has('nvim-0.5')
lua << EOF
require'nvim-treesitter.configs'.setup {
	highlight = { enable = true },
	textobjects = { enable = true },
	indent = { enable = true },
}
EOF
endif

" }}}
" }}}
" vim: foldmethod=marker foldlevel=0
