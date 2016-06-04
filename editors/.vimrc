"{{{ ====================== color ======================

color molokai
"color monokai

"}}}

"{{{ ====================== commands ======================

command! -nargs=0 Clean :set nonu nolist foldcolun=0
command! -nargs=0 Unclean :set nu list foldcolun=1

"}}}

"{{{ ====================== functions ======================

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

"}}}

"{{{ ====================== mappings ======================

" Leader
let mapleader = 'ç'

" NerdTree Ctrl n
map <C-n> :NERDTreeToggle<CR>

" Force to use h/j/k/l and arrows move line
"nnoremap <left>  :echoe "use j"<cr>
"nnoremap <right> :echoe "use ç"<cr>
"nnoremap <up> 	 :echoe "use k"<cr>
"nnoremap <down>  :echoe "use l"<cr>

" bether navegation for brazilian keybord
nmap j <left>
nmap k <down>
nmap l <up>
nmap ç <right>
vmap j <left>
vmap ç <right>
vmap l <up>
vmap k <down>


" Move a line of text using ALT+[jk]
nmap <M-j> mz:m+<cr>`z 
nmap <M-k> mz:m-2<cr>`z 
vmap <M-j> m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" Smart way to move between windows
map <C-j> <C-W>k
map <C-k> <C-W>l
map <C-h> <C-W>j
map <C-l> <C-W>ç

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove

" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell<cr>
map <leader>ns :setlocal nospell<cr>

" Visual mode pressing * or # searches for the current selection
vnoremap <silent> * :call VisualSelection('f')<CR>
vnoremap <silent> # :call VisualSelection('b')<CR>

" Create new line and stay in normal mode
nmap <Leader>o o<ESC>k
nmap <Leader>O O<ESC>

" Quick pairs
imap <leader>' ''<ESC>i
imap <leader>" ""<ESC>i
imap ( ()<ESC>i
imap [ []<ESC>i
imap { {}<ESC>i
"imap {<CR> <esc>lxo}<esc>O

" Without pairs
imap <leader>( (
imap <leader>[ [
imap <leader>{ {

" Change between insert and Paste
set pastetoggle=<F2>

" Change color scheme
map <F6> :call <SID>SwitchColorSchemes()<CR>:echo g:colors_name<CR>

"}}}

"{{{ ====================== settings ======================

"{{{********************* Vundle and Plugins **************************

set nocompatible  " be iMproved
filetype off
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Bundle 'powerline/powerline', {'rtp': 'powerline/bindings/vim/'}
Plugin 'Lokaltog/vim-easymotion'
Plugin 'Valloric/YouCompleteMe'
Plugin 'christoomey/vim-system-copy'
Plugin 'ervandew/supertab'
Plugin 'gmarik/Vundle.vim'
Plugin 'jszakmeister/vim-togglecursor'
Plugin 'kana/vim-textobj-entire'
Plugin 'kana/vim-textobj-function'
Plugin 'kana/vim-textobj-indent'
Plugin 'kana/vim-textobj-line'
Plugin 'kana/vim-textobj-user'
Plugin 'kien/ctrlp.vim'
Plugin 'lervag/vimtex'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'vim-scripts/ReplaceWithRegister'
Plugin 'vim-scripts/ZoomWin'

call vundle#end()

filetype plugin indent on
"}}}
"
"{{{*******************************************************************************

set ai		  	  " Auto indent
set autoread	  " Set to auto read when a file is changed from the outside
set autowrite     " Automatically :write before running commands
set backspace=2   " Backspace deletes like most programs in insert mode
set backup
set colorcolumn=+1	" Make it obvious where 80 characters is
set complete+=kspell " Extends dictionary
set cursorline    " Highlight cursor line
set encoding=utf8 " Set utf8 as standard encoding and en_US as the standard language
set ffs=unix,dos,mac " Use Unix as the standard file type
set foldmethod=marker "Método das dobras
set history=700
set hlsearch	  " Highlight search results
set ignorecase	  " Ignore case when searching
set incsearch     " Makes search act like search in modern browsers
set laststatus=2  " Always display the status line
set lazyredraw    " Don't redraw while executing macros (good performance config)
set lbr
set list listchars=tab:»·,trail:-,extends:>,precedes:<,eol:¬,nbsp:·
set mat=2	  	  " How many tenths of a second to blink when matching brackets
set mouse=a		  " enable mouse
set noswapfile
set number	  	  " Numbers
set numberwidth=5
set omnifunc=syntaxcomplete#Complete " omnicompletition
set relativenumber " Show numbers relative to the current line
set ruler		  " Always show current position
set scrolloff=10  " Keep cursor centered
set shiftwidth=4
set showcmd		  " Show current comands
set showmatch	  " Show matching brackets when text indicator is over them
set si 		  	  " Smart indent
set smartcase	  " When searching try to be smart about cases 
set smarttab	  " Be smart when using tabs
set tabstop=4	  " 1 tab == 4 spaces
set textwidth=100
set viminfo^=% 	  " Remember info about open buffers on close
set wildmenu	  " Turn on the WiLd menu
set writebackup
set backupdir=~/Documents/swap_files

syntax enable	  " Enable syntax highlighting

"}}}

" ----------------------------------------------------------------------------
" when use GNU/Linux use bash as shell 
"{{{ -------------------------------------------------------------------------

if has("unix")
	let &shell="bash"
    set clipboard=autoselect
endif  
"}}}
"
" ----------------------------------------------------------------------------
" Return to last edit position when opening files ans save history
"{{{ ----------------------------------------------------------------------------

set viminfo='100,\"1000,:40,%,n~/.viminfo
   au BufReadPost * if line("'\"")|execute("normal `\"")|endif
   autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
"}}}
"
" ----------------------------------------------------------------------------
" Manager Cursor color
" {{{----------------------------------------------------------------------------

autocmd VimEnter * silent !echo -ne "\033]12;red\007"
autocmd InsertEnter * silent !echo -ne "\033]12;blue\007"
autocmd InsertLeave * silent !echo -ne "\033]12;red\007"
silent !echo -ne "\033]12;rede\007"
" reset cursor when vim exits
autocmd VimLeave * silent !echo -ne "\033]12\007"

"}}}
"
"}}}
