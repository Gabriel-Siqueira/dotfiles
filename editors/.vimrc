"{{{ ====================== Commands ======================

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
         colorscheme monokai
    elseif g:colors_name == 'monokai'
         colorscheme desert
    elseif g:colors_name == 'desert'
        colorscheme vividchalk
    elseif g:colors_name == 'vividchalk'
        colorscheme solarized
        let g:solarized_termcolors=256
    elseif g:colors_name == 'solarized'
        colorscheme monokai
        colorscheme molokai
    endif
endfunction

"}}}

"{{{ ====================== mappings ======================

" Leader
let mapleader = 'ç'
set tm=2000 " time to leader became ç

" NerdTree Ctrl n
map <C-n> :NERDTreeToggle<CR>

" Move a line of text using ALT+[jk]
nmap <leader>j mz:m+<cr>`z
nmap <leader>k mz:m-2<cr>`z
vmap <leader>j m'>+<cr>`<my`>mzgv`yo`z
vmap <leader>k :m'<-2<cr>`>my`<mzgv`yo`z

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove

" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" Visual mode pressing * or # searches for the current selection
vnoremap <silent> * :call VisualSelection('f')<CR>
vnoremap <silent> # :call VisualSelection('b')<CR>

" Create new line and stay in normal mode
nmap <Leader>o o<ESC>k
nmap <Leader>O O<ESC>

" Change between insert and Paste
set pastetoggle=<F2>

" Change color scheme
map <F6> :call <SID>SwitchColorSchemes()<CR>:echo g:colors_name<CR>

" remove extra whitespace
nmap <leader><space> :%s/\s\+$<cr>

" Prompt for comand with vimux
nmap <Leader>p :VimuxPromptCommand<CR>

" Toggle Hard mode
nnoremap <leader>h <Esc>:call ToggleHardMode()<CR>

" ultisnips and ycm {{{
" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
"}}}

"}}}

"{{{ ===================== Plugins ==========================

set nocompatible  " be iMproved

filetype off
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'Lokaltog/vim-easymotion'                   " move following leters
Plugin 'Valloric/YouCompleteMe'                    " auto-completition
Plugin 'benmills/vimux'                            " use tmux with vim
Plugin 'ctrlpvim/ctrlp.vim'                        " finder (fuzzy file, tag, etc)
Plugin 'ervandew/supertab'                         " tab for complete
Plugin 'gmarik/Vundle.vim'                         " manage plugins
Plugin 'honza/vim-snippets'                        " snippets for ultisnips
Plugin 'jiangmiao/auto-pairs'                      " add pairs automaticaly
Plugin 'kana/vim-textobj-entire'                   " new object
Plugin 'kana/vim-textobj-function'                 " new object
Plugin 'kana/vim-textobj-indent'                   " new object
Plugin 'kana/vim-textobj-line'                     " new object
Plugin 'kana/vim-textobj-user'                     " new object
Plugin 'lervag/vimtex'                             " for edit latex
Plugin 'ryanoasis/vim-devicons'                    " icons
Plugin 'scrooloose/nerdtree'                       " tree of files
Plugin 'scrooloose/syntastic'                      " tree of files
"Plugin 'Shougo/neocomplete.vim'                    " auto-completition
Plugin 'SirVer/ultisnips'                          " use snippets
Plugin 'tpope/vim-commentary'                      " comment in and out
Plugin 'tpope/vim-fugitive'                        " work with git
Plugin 'tpope/vim-repeat'                          " extend use of .
Plugin 'tpope/vim-surround'                        " new object surrond
Plugin 'vim-airline/vim-airline'                   " new mode line
Plugin 'vim-airline/vim-airline-themes'            " themes for airline
Plugin 'vim-scripts/ZoomWin'                       " make pane full screen
Plugin 'wikitopian/hardmode'                       " make pane full screen

""{{{ Colors
Plugin 'altercation/vim-colors-solarized'
Plugin 'tomasr/molokai'
Plugin 'sickill/vim-monokai'
Plugin 'tpope/vim-vividchalk'
""}}}

"{{{ Haskell
Plugin 'neovimhaskell/haskell-vim'
" Plugin 'enomsg/vim-haskellConcealPlus'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'eagletmt/neco-ghc'
Plugin 'Twinside/vim-hoogle'
Plugin 'mpickering/hlint-refactor-vim'
"}}}

call vundle#end()

filetype plugin indent on
"}}}

"{{{ ====================== settings ======================

syntax enable      " Enable syntax highlighting

" Sets {{{

"{{{ indentation
set ai               " Auto indent
set si               " Smart indent
set tabstop=4        " 1 tab == 2 spaces
set shiftwidth=4     " Number of spaces to use for each step of (auto)indent
set expandtab        " Replace tabs with spaces
"}}}

"{{{ numbers
set number            " Line number
set ruler            " Always show current position
set numberwidth=2
set relativenumber " Show numbers relative to the current line
"}}}

"{{{ search
set hlsearch      " Highlight search results
set incsearch     " Makes search act like search in modern browsers
"}}}

"{{{ line and column
set cursorline    " Highlight cursor line
set colorcolumn=+1    " Make it obvious where 80 characters is
"}}}

"{{{ play nice
set backspace=2   " Backspace deletes like most programs in insert mode
set mouse=a          " enable mouse
set scrolloff=10  " Keep cursor centered
set showmatch      " Show matching brackets when text indicator is over them
set tw=0          " wrap but not brake line
set textwidth=77
set viminfo^=%       " Remember info about open buffers on close
"}}}
"
"{{{ files (save, read, ...)
set autoread      " Set to auto read when a file is changed from the outside
set autowrite     " Automatically :write before running commands
set backup
set writebackup
set backupdir=~/Documents/swap_files
"}}}

"{{{ spell and language
set complete+=kspell " Extends dictionary
set encoding=utf8 " Set utf8 as standard encoding and en_US as the standard language
"}}}

"{{{ history
set history=7000
"}}}

"{{{ Folds
augroup vimrcFold
  " fold vimrc itself by categories
  autocmd!
  autocmd FileType vim set foldmethod=marker
  autocmd FileType vim set foldlevel=0
augroup END
set foldmethod=indent
set foldlevelstart=99
set foldcolumn=1
set foldlevel=1
"}}}

"{{{ Others
set ffs=unix,dos,mac  " Use Unix as the standard file type
set laststatus=2  " Always display the status line
set lazyredraw    " Don't redraw while executing macros (good performance)
set list listchars=tab:»·,trail:-,extends:>,precedes:<,eol:¬,nbsp:·
set omnifunc=syntaxcomplete#Complete " omnicompletition
set showcmd          " Show current commands
set wildmenu      " Turn on the WiLd menu
"}}}

"}}}

" Plugins {{{

" hard mode default
" autocmd VimEnter,BufNewFile,BufReadPost * silent! call HardMode()

" airline {{{
let g:airline_powerline_fonts = 1
let g:airline_theme='jellybeans'
"}}}

"}}}

" when use GNU/Linux use bash as shell
"{{{

if has("unix")
    let &shell="bash"
    set clipboard=autoselect
endif
"}}}

" Return to last edit position when opening files ans save history
"{{{

set viminfo='100,\"1000,:40,%,n~/.viminfo
   au BufReadPost * if line("'\"")|execute("normal `\"")|endif
   autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
"}}}

" Manager Cursor color
" {{{
if exists('$TMUX')
else
    autocmd VimEnter * silent !echo -ne "\033]12;blue\007"
    autocmd InsertEnter * silent !echo -ne "\033]12;green\007"
    autocmd InsertLeave * silent !echo -ne "\033]12;blue\007"
    silent !echo -ne "\033]12;blue\007"
    " reset cursor when vim exits
    autocmd VimLeave * silent !echo -ne "\033]12\007"
endif
"}}}

"{{{ color

color monokai
color molokai
set t_ut=

"}}}
