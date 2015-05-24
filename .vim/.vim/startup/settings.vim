"*************************************** Require to Vundle *******************************************

set nocompatible  " be iMproved
filetype off
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" let Vundle manage Vundle
Plugin 'gmarik/Vundle.vim'
" All of your Plugins must be added before the following line
call vundle#end()
filetype plugin indent on

"*****************************************************************************************************

set backspace=2   " Backspace deletes like most programs in insert mode
set cursorline    " Highlight cursor line
set mouse=a		  " enable mouse
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
set ruler	  " Always show current position
set ignorecase	  " Ignore case when searching
set smartcase	  " When searching try to be smart about cases 
set hlsearch	  " Highlight search results
set incsearch     " Makes search act like search in modern browsers
set lazyredraw    " Don't redraw while executing macros (good performance config)
set showmatch	  " Show matching brackets when text indicator is over them
set mat=2	  " How many tenths of a second to blink when matching brackets
set textwidth=80
set colorcolumn=+1 " Make it obvious where 80 characters is
set number	  " Numbers
set numberwidth=5
set encoding=utf8 " Set utf8 as standard encoding and en_US as the standard language
set ffs=unix,dos,mac " Use Unix as the standard file type
set smarttab	  " Be smart when using tabs
set shiftwidth=4
set tabstop=4	  " 1 tab == 4 spaces
set lbr
set tw=500	  " Linebreak on 500 characters
set ai		  " Auto indent
set si 		  " Smart indent

"************************************** Spell ***************************************

set complete+=kspell

"************************************************************************************

" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif
" Remember info about open buffers on close
set viminfo^=%

syntax enable	  " Enable syntax highlighting
