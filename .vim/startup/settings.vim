"*************************************** Vundle and Plugins *******************************************

set nocompatible  " be iMproved
filetype off
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-fugitive'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'jszakmeister/vim-togglecursor'
Bundle 'powerline/powerline', {'rtp': 'powerline/bindings/vim/'}

call vundle#end()

filetype plugin indent on

"*******************************************************************************************************

set backspace=2   " Backspace deletes like most programs in insert mode
set cursorline    " Highlight cursor line
set mouse=a		  " enable mouse
set scrolloff=10  " Keep cursor centered
set list listchars=tab:»·,trail:-,extends:>,precedes:<,eol:¬,nbsp:·
set backup
set writebackup
set noswapfile
set history=700
set ruler         " show the cursor position all the time
set laststatus=2  " Always display the status line
set showcmd       " display incomplete commands
set incsearch     " do incremental searching
set autowrite     " Automatically :write before running commands
set autoread	  " Set to auto read when a file is changed from the outside
set wildmenu	  " Turn on the WiLd menu
set ruler		  " Always show current position
set ignorecase	  " Ignore case when searching
set smartcase	  " When searching try to be smart about cases 
set hlsearch	  " Highlight search results
set incsearch     " Makes search act like search in modern browsers
set lazyredraw    " Don't redraw while executing macros (good performance config)
set showmatch	  " Show matching brackets when text indicator is over them
set mat=2	  	  " How many tenths of a second to blink when matching brackets
set textwidth=80
set colorcolumn=+1	" Make it obvious where 80 characters is
set number	  	  " Numbers
set numberwidth=5
set encoding=utf8 " Set utf8 as standard encoding and en_US as the standard language
set ffs=unix,dos,mac " Use Unix as the standard file type
set smarttab	  " Be smart when using tabs
set shiftwidth=4
set tabstop=4	  " 1 tab == 4 spaces
set lbr
set tw=500	  	  " Linebreak on 500 characters
set ai		  	  " Auto indent
set si 		  	  " Smart indent
set viminfo^=% 	  " Remember info about open buffers on close
set showcmd		  " Show current comands
set foldmethod=marker "Método das dobras

syntax enable	  " Enable syntax highlighting

"************************************** Spell ***************************************

set complete+=kspell

"************************************************************************************

" ----------------------------------------------------------------------------
" Definindo o bash para o GNU/Linux
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
