"*************************************** Vundle and Plugins *******************************************

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

"*******************************************************************************************************

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
set backupdir = ~/Documents/swap_files

syntax enable	  " Enable syntax highlighting

" ----------------------------------------------------------------------------
" when use GNU/Linux use bash as shell 
" ----------------------------------------------------------------------------

if has("unix")
	let &shell="bash"
    set clipboard=autoselect
endif  

" ----------------------------------------------------------------------------
" Return to last edit position when opening files ans save history
" ----------------------------------------------------------------------------

set viminfo='100,\"1000,:40,%,n~/.viminfo
   au BufReadPost * if line("'\"")|execute("normal `\"")|endif
   autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

" ----------------------------------------------------------------------------
" Manager Cursor color
" ----------------------------------------------------------------------------

autocmd VimEnter * silent !echo -ne "\033]12;red\007"
autocmd InsertEnter * silent !echo -ne "\033]12;blue\007"
autocmd InsertLeave * silent !echo -ne "\033]12;red\007"
silent !echo -ne "\033]12;rede\007"
" reset cursor when vim exits
autocmd VimLeave * silent !echo -ne "\033]12\007"
