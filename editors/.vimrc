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
        colorscheme molokai
    endif
endfunction

"}}}

"{{{ ====================== mappings ======================

" Leader
let mapleader = 'ç'
set tm=2000

" NerdTree Ctrl n
map <C-n> :NERDTreeToggle<CR>

" Move a line of text using ALT+[jk]
nmap <A-j> mz:m+<cr>`z
nmap <A-k> mz:m-2<cr>`z
vmap <A-j> m'>+<cr>`<my`>mzgv`yo`z
vmap <A-k> :m'<-2<cr>`>my`<mzgv`yo`z

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
"}}}

"{{{ ===================== Vundle and Plugins ==========================

set nocompatible  " be iMproved
filetype off
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'Lokaltog/vim-easymotion'                   " move following leters
"Plugin 'Valloric/YouCompleteMe'                    " auto-completition
Plugin 'benmills/vimux'                            " use tmux with vim
Plugin 'gmarik/Vundle.vim'                         " manage plugins
Plugin 'honza/vim-snippets'                        " snippets for ultisnips
Plugin 'jiangmiao/auto-pairs'                      " add pairs automaticaly
Plugin 'kana/vim-textobj-entire'                   " new object
Plugin 'kana/vim-textobj-function'                 " new object
Plugin 'kana/vim-textobj-indent'                   " new object
Plugin 'kana/vim-textobj-line'                     " new object
Plugin 'kana/vim-textobj-user'                     " new object
Plugin 'kien/ctrlp.vim'                            " fuzy find
Plugin 'lervag/vimtex'                             " for edit latex
Plugin 'scrooloose/nerdtree'                       " tree of files
Plugin 'Shougo/neocomplete.vim'                    " auto-completition
Plugin 'SirVer/ultisnips'                          " use snippets
Plugin 'tpope/vim-commentary'                      " comment in and out
Plugin 'tpope/vim-fugitive'                        " work with git
Plugin 'tpope/vim-repeat'                          " extend use of .
Plugin 'tpope/vim-surround'                        " new object surrond
Plugin 'vim-airline/vim-airline'                   " new mode line
Plugin 'vim-airline/vim-airline-themes'            " themes for airline
Plugin 'vim-scripts/ZoomWin'                       " make pane full screen

"{{{ Haskell
Plugin 'neovimhaskell/haskell-vim'
Plugin 'enomsg/vim-haskellConcealPlus'
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

" Auto-completes {{{

" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" let g:ycm_filetype_whitelist = { 'c': 1 }
" autocmd FileType c NeoCompleteLock
" let g:ycm_filetype_whitelist = { 'cpp': 1 }
" autocmd FileType cpp NeoCompleteLock
" let g:ycm_filetype_whitelist = { 'c#': 1 }
" autocmd FileType c# NeoCompleteLock
" let g:ycm_filetype_whitelist = { 'python': 1 }
" autocmd FileType python NeoCompleteLock
" let g:ycm_filetype_whitelist = { 'go': 1 }
" autocmd FileType go NeoCompleteLock
" let g:ycm_filetype_whitelist = { 'typescript': 1 }
" autocmd FileType typescript NeoCompleteLock
" let g:ycm_filetype_whitelist = { 'javascript': 1 }
" autocmd FileType javascript NeoCompleteLock
" let g:ycm_filetype_whitelist = { 'rust': 1 }
" autocmd FileType rust NeoCompleteLock
" }}}

" airline {{{
let g:airline_powerline_fonts = 1
let g:airline_theme='jellybeans'
"}}}

" ultisnips {{{
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"
let g:UltiSnipsSnippetDirectories=["UltiSnips", "mysnippets"]
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
