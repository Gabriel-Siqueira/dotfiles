"{{{ ===================== Plugging ========================
if has('nvim')
    call plug#begin('~/.local/share/nvim/plugged')
else
    call plug#begin('~/.vim/plugged')
endif
" Shipped with vim {{{

if !has('nvim')
    packadd! matchit
endif

" }}}
" Improve vim language {{{

" objects {{{

Plug 'kana/vim-textobj-user'                  " help define user objects

Plug 'bkad/CamelCaseMotion'                   " CamelCase word
Plug 'kana/vim-smartword'                     " smart word
Plug 'kana/vim-textobj-entire'                " the hole file
Plug 'kana/vim-textobj-fold'                  " folded lines
Plug 'kana/vim-textobj-indent'                " indentation block
Plug 'kana/vim-textobj-line'                  " line
Plug 'lucapette/vim-textobj-underscore'       " underscore (_)
Plug 'sgur/vim-textobj-parameter'             " parameter of a function
Plug 'thinca/vim-textobj-between'             " between characters
Plug 'tpope/vim-surround'                     " surround
Plug 'zandrmartin/vim-textobj-blanklines'     " blank lines

" }}}
" operators {{{

Plug 'kana/vim-operator-user'                 " help define user operators

Plug 'AndrewRadev/splitjoin.vim'              " split and join expressions
Plug 'christoomey/vim-sort-motion'            " sort
Plug 'tpope/tpope-vim-abolish'                " Abbreviate, Substitute and Coerce
Plug 'tpope/vim-commentary'                   " comment in and out
Plug 'tpope/vim-speeddating'                  " fast way of change date
Plug 'tpope/vim-unimpaired'                   " complementary pairs of mappings
Plug 'vim-scripts/ReplaceWithRegister'        " one command to d them p

" }}}
" Motions {{{

Plug 'Lokaltog/vim-easymotion'                " move following letters
Plug 'jeetsukumaran/vim-indentwise'           " indentation level

" }}}
" other {{{

Plug 'kana/vim-niceblock'                     " I and A in all visual and better transition of visual
Plug 'nelstrom/vim-visual-star-search'        " * and # search for visual blocks
Plug 'kana/vim-submode'                       " help define submodules
Plug 'tpope/vim-repeat'                       " extend use of .

" }}}

" }}}
" Languages/file types {{{

Plug 'sheerun/vim-polyglot'
"{{{ Erlang

" Plug 'vim-erlang/erlang-motions.vim'           " motion and text objects
" Plug 'vim-erlang/vim-erlang-compiler'          " compiler and syntax checking
" Plug 'vim-erlang/vim-erlang-omnicomplete'      " omnicompletion

" }}}
"{{{ Haskell

" Plug 'Twinside/vim-haskellFold'                " fold for haskell
" Plug 'Twinside/vim-hoogle'                     " search on hoogle
" Plug 'dag/vim2hs'                              " lots of help with haskell
" Plug 'eagletmt/ghcmod-vim', {'do' : 'cabal install ghc-mode'} " type checker

"}}}
" {{{ Other languages and file types

Plug 'jceb/vim-orgmode'                       " for org
" Plug 'python-mode/python-mode'                " for python
" Plug 'rust-lang/rust.vim'                     " for rust
" Plug 'PotatoesMaster/i3-vim-syntax'           " for i3 config
" Plug 'justmao945/vim-clang'                   " for c,cpp files
" Plug 'lervag/vimtex'                          " for edit latex
" Plug 'plasticboy/vim-markdown'                " for markdown
" Plug 'the-lambda-church/coquille'             " for coq
" Plug 'tmux-plugins/vim-tmux'                  " for tmux.conf

" }}}

" }}}
" Autocomplete and snippets {{{

" if has('nvim')
  " Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" else
  " Plug 'Shougo/deoplete.nvim'
  " Plug 'roxma/nvim-yarp'
  " Plug 'roxma/vim-hug-neovim-rpc'
" endif

" Plug 'eagletmt/neco-ghc'                      " for haskell (deoplete)
" Plug 'poppyschmo/deoplete-latex'              " for latex (deoplete)
" Plug 'zchee/deoplete-clang'                   " for  C/C++ (deoplete)
" Plug 'zchee/deoplete-jedi'                    " for python (deoplete)
" Plug 'zchee/deoplete-zsh'                     " for zsh (deoplete)

" Plug 'SirVer/ultisnips'                       " use snippets
" Plug 'honza/vim-snippets'                     " more snippets

" }}}
" Other editing/syntax {{{

" Plug 'dhruvasagar/vim-table-mode'             " create and edit tables
Plug 'godlygeek/tabular'                      " align text (:Tab[ularize])
Plug 'jiangmiao/auto-pairs'                   " add pairs automatically
Plug 'rhysd/vim-grammarous'                   " grammar checking
Plug 'w0rp/ale'                               " use linter while typing

" }}}
" Interface {{{

if !has('nvim')
    Plug 'vim-utils/vim-man'                      " view man pages
    Plug 'osyo-manga/vim-over'                    " highlight substitute pattern
endif

" Plug 'Shougo/defx.nvim'                       " tree of files
Plug 'Konfekt/FastFold'                       " fold/unfold on right times
Plug 'Shougo/denite.nvim'                     " search/display info (file, buf)
Plug 'juanpabloaj/help.vim'                   " better navigation in help
" Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
" Plug 'junegunn/fzf.vim'                       " fuzzy finder on vim
Plug 'hkupty/iron.nvim'                       " send commands to repl
Plug 'scrooloose/nerdtree'                    " tree of files
Plug 'seagoj/last-position.vim'               " save cursor position
Plug 'sjl/gundo.vim'                          " undo tree
Plug 'troydm/zoomwintab.vim'                  " make pane full screen
Plug 'vim-airline/vim-airline'                " new mode line
Plug 'vim-airline/vim-airline-themes'         " themes for airline

" }}}
" Applications interface {{{

if has('nvim')
    Plug 'hkupty/iron.nvim', { 'do': ':UpdateRemotePlugins' }
endif

" Plug 'blindFS/vim-taskwarrior'           " work with taskwarrior
Plug 'tpope/vim-fugitive'                " work with git
Plug 'szw/vim-tags'                      " automatic generate tags on save

" Tmux
Plug 'christoomey/vim-tmux-navigator'    " same navigation vim and tmux
Plug 'christoomey/vim-tmux-runner'       " send commands/code from vim to tmux
Plug 'roxma/vim-tmux-clipboard'          " same clipboard vim and tmux

" }}}
""{{{ Colors

Plug 'NLKNguyen/papercolor-theme'
Plug 'gosukiwi/vim-atom-dark'
Plug 'lifepillar/vim-solarized8'
Plug 'tomasr/molokai'

""}}}
" Others {{{

Plug 'Shougo/vimproc.vim', {'do' : 'make'}     " interactive command execution (for ghc-mode)
Plug 'let-def/vimbufsync'                      " coquille uses it
Plug 'tpope/vim-obsession'                     " automatic save session
Plug 'lambdalisue/suda.vim'                    " use sudo on comand line

" }}}
call plug#end()
"}}}
"{{{ ====================== Commands =======================

command! -nargs=0 Clean   :set nonu norelativenumber nolist foldcolumn=0
command! -nargs=0 Unclean :set nu relativenumber list foldcolumn=1
command! -nargs=0 Envim   :e ~/.config/nvim/init.vim
command! -nargs=0 Evim    :e ~/.vimrc
command! -nargs=0 So      :so $MYVIMRC | set foldmethod=marker | set foldlevel=0

"}}}
"{{{ ====================== Functions ======================
"{{{ SwitchSpellLang

let g:myLang = 0
let g:myLangList = ['pt_br', 'de_de', 'en']
function! SwitchSpellLang()
  "loop through languages
  let &l:spelllang = g:myLangList[g:myLang] | setlocal spell
  echomsg 'language:' g:myLangList[g:myLang]
  let g:myLang = g:myLang + 1
  if g:myLang >= len(g:myLangList) | let g:myLang = 0 | endif
endfunction

"}}}
"}}}
"{{{ ====================== Mappings =======================
" leader {{{

let mapleader = '\
set tm=2000 " time to leader became ç

" }}}
" terminal {{{

if has('nvim')
    tnoremap <C-\> <C-\><C-n>
    tnoremap <C-h> <C-\><C-n><C-w>h
    tnoremap <C-j> <C-\><C-n><C-w>j
    tnoremap <C-k> <C-\><C-n><C-w>k
    tnoremap <C-l> <C-\><C-n><C-w>l
    tnoremap <C-space> <C-\><C-n>:b#<CR>

    " terminal in a new tab
    noremap  <Leader>cc :term<CR>
    " terminal in a vertical split
    noremap  <Leader>cv :vsp<CR><C-w><C-w>:term<CR>
    " terminal in a horizontal split
    noremap  <Leader>cs :sp<CR><C-w><C-w>:term<CR>
    " paste from register
    tnoremap <expr> <A-r> '<C-\><C-N>"'.nr2char(getchar()).'pi'
endif

" }}}
" do stuff quickly {{{
noremap <leader>u :update<CR>
noremap <C-space> :b#<CR>
" }}}
" move {{{

noremap  k gk
noremap  j gj
noremap  0 g0
noremap  $ g$
noremap  gk k
noremap  gj j
noremap  g0 0
noremap  g$ $

" }}}
" search {{{

nmap <leader>n :noh<cr>

" }}}
" tabs {{{

nmap <leader>tn :tabnew<cr>
nmap <leader>to :tabonly<cr>
nmap <leader>tc :tabclose<cr>
nmap <leader>tm :tabmove<cr>

" }}}
" tags {{{

nmap <leader>tg :!ctags -R .<cr>

" }}}
" spell checking {{{

nmap <leader>ss :setlocal spell!<cr>
nmap <leader>sc :call SwitchSpellLang()<CR>

" }}}
" <Fx> {{{

noremap <F1> :help
set pastetoggle=<F2>               " Change between insert and Paste
noremap <F3> :GundoToggle<CR>      " toggle graphic undo tree
noremap <F4> :NERDTreeToggle<CR>   " Toggle File Explorer

" }}}
" {{{ plugins

" deoplete {{{

" inoremap <silent><expr> <TAB>
" \ pumvisible() ? "\<C-n>" :
" \ <SID>check_back_space() ? "\<TAB>" :
" \ deoplete#mappings#manual_complete()
" function! s:check_back_space() abort "{{{
" let col = col('.') - 1
" return !col || getline('.')[col - 1]  =~ '\s'
" endfunction"}}}

" }}}
" ultisnips {{{

" let g:UltiSnipsExpandTrigger = "<c-y>"
" let g:UltiSnipsJumpForwardTrigger = "<c-f>"
" let g:UltiSnipsJumpBackwardTrigger = "<c-b>"

"}}}
" Iron {{{
nmap <leader>i <Plug>(iron-send-motion)
vmap <leader>i <Plug>(iron-send-motion)
nmap <leader>is :IronRepl<CR>
nmap <leader>ir <Plug>(iron-repeat-cmd)
" }}}
" smartwords and camelcasemotion {{{

map w  <Plug>(smartword-w)
map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
map ge  <Plug>(smartword-ge)

noremap <leader>w  w
noremap <leader>b  b
noremap <leader>e  e
noremap <leader>ge ge

call camelcasemotion#CreateMotionMappings('<leader>c')

" }}}
" {{{ vim-tmux-runner

nnoremap <leader>rr  :VtrResizeRunner<cr>
nnoremap <leader>rt  :VtrReorientRunner<cr>
nnoremap <leader>rs  :VtrSendCommandToRunner<cr>
nnoremap <leader>rl  :VtrSendLinesToRunner<cr>
nnoremap <leader>ro  :VtrOpenRunner<cr>
nnoremap <leader>rk  :VtrKillRunner<cr>
nnoremap <leader>rf  :VtrFocusRunner<cr>
nnoremap <leader>rd  :VtrDetachRunner<cr>
nnoremap <leader>ra  :VtrReattachRunner<cr>
nnoremap <leader>re  :VtrClearRunner<cr>
nnoremap <leader>rf  :VtrFlushCommand<cr>
vnoremap <leader>rl  :VtrSendLinesToRunner<cr>

" }}}
" FZF {{{

" noremap  <C-b> :Buffers<CR>
" nnoremap <C-p>  :FZF<cr>
" tnoremap <C-p>  <C-\><C-n>:FZF<cr>
" tnoremap <C-b>  <C-\><C-n>:Buffers<cr>

" }}}
" zoomwintab {{{
nnoremap <leader>z :ZoomWinTabToggle<CR>
" }}}

"}}}
" {{{ accept habits

cnoreabbrev Q q
cnoreabbrev W w
cnoreabbrev WQ wq
cnoreabbrev Wq q
cnoreabbrev X x
cnoreabbrev xx X
nnoremap Y y$

" }}}
"}}}
"{{{ ====================== Settings =======================

set nocompatible   " be iMproved
syntax enable      " Enable syntax highlighting
syntax on
filetype plugin on    " allow file types plugins to run when opening file
" ----------------- Sets ------------------  {{{
"{{{ indentation

set ai               " Auto indent
set si               " Smart indent
set expandtab        " Replace tabs with spaces
set shiftwidth=4     " When using >> <<
set tabstop=4        " Size of tab character

"}}}
"{{{ numbers

set number           " Line number
set ruler            " Always show current position
set numberwidth=2
set relativenumber   " Show numbers relative to the current line

"}}}
"{{{ search

set hlsearch      " Highlight search results
set incsearch     " Show result while typing

"}}}
"{{{ line and column

set cursorline         " Highlight cursor line
set colorcolumn=80     " Highlight textwith column
set textwidth=0        " Do not break lines
if has('linebreak')
    set wrap             " make virtual lines
    set linebreak        " wrap long lines at characters in 'breakat'
    let &showbreak='⤷ '  " character for line break (U+2937, UTF-8: E2 A4 B7)
endif

"}}}
"{{{ mouse and cursor

set mouse=a                     " enable mouse
set mousemodel=popup            " right button open pupup menu
set scrolloff=10                " Keep cursor centered
set sidescrolloff=3             " scrolloff for columns
set showmatch                   " Show matching brackets when text indicator is over them

"}}}
"{{{ play nice

set backspace=indent,start,eol  " Backspace deletes like most programs in insert mode
" set viminfo^=%                  " Remember info about open buffers on close
set formatoptions+=j            " remove comment leader when joining comment lines
set hidden                      " Do not need to save buffers to open others
if has('termguicolors')
    set termguicolors
endif

"}}}
"{{{ files (save, read, ...)

set autoread      " Set to auto read when a file is changed from the outside
set autowrite     " Automatically :write before running commands
set backup
set writebackup

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

"}}}
"{{{ spell, language and complete

set infercase        " when using upper case complete only with upper case
set encoding=utf8    " Set utf8 as standard encoding

"}}}
"{{{ history

set history=7000

"}}}
"{{{ folds

if has('folding')
    set foldmethod=indent
    set foldlevelstart=99   " start unfolded
    set foldcolumn=1
    set foldlevel=1
endif

"}}}
" {{{ nvim specific

if exists('&inccommand')
  set inccommand=split
endif

" }}}
"{{{ others

set splitright
set noshowmode        " remove text insert from command line
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

"}}}
"}}}
" ----------------- File Specific --------------- {{{

let g:tex_flavor = "latex"
autocmd Filetype haskell setlocal expandtab " always use spaces on haskell
:au BufEnter * if &buftype == 'terminal' | :startinsert | endif

" }}}
" ----------------- Plugins --------------- {{{
" Enable/disable on startup {{{

let g:deoplete#enable_at_startup = 1
let g:acp_enableAtStartup = 0    " vim autocomplete
let g:echodoc_enable_at_startup = 1

" }}}
" airline {{{

let g:airline_powerline_fonts = 1
let g:airline_theme='jet'
let g:airline#extensions#capslock#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#wordcount#enabled = 1
let g:airline#extensions#tmuxline#enabled = 0
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#vimtex#enabled = 1
let g:airline#extensions#ale#enabled = 1
" let g:airline_left_sep=''
" let g:airline_right_sep=''
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = '|'
" let g:airline#extensions#tabline#right_sep = ''
" let g:airline#extensions#tabline#right_alt_sep = '|'

"}}}
" deoplete {{{

" let g:deoplete#disable_auto_complete = 1
" call deoplete#custom#source('_',
" \ 'matchers', ['matcher_full_fuzzy'])

" }}}
" iron {{{
let g:iron_repl_open_cmd = "vsplit"
" }}}
" ultisnips {{{

" if has('nvim')
"     let g:UltiSnipsSnippetsDir = "~/.config/nvim/UltiSnips"
" else
"     let g:UltiSnipsSnippetsDir = "~/.vim/UltiSnips"
" endif
" let g:UltiSnipsSnippetDirectories=["UltiSnips","vim-snippets"]
" let g:UltiSnipsEditSplit="context"

" }}}
" vim2hs {{{

let g:haskell_conceal = 0 " disable all prity haskell symbols
" let g:haskell_conceal_wide = 1 " enable all prity haskell symbols

" }}}
" {{{ vim-tmux-runner

let g:VtrStripLeadingWhitespace = 0
let g:VtrClearEmptyLines = 0
let g:VtrAppendNewline = 1
let g:VtrOrientation = "h"
let g:VtrPercentage = 50

" }}}
" netrw {{{

let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=4  " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_list_hide=netrw_gitignore#Hide() " do not show git ignored
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

" }}}
" vimfiler {{{

let g:vimfiler_as_default_explorer = 1

" }}}
" vimpolyglot {{{
" let g:polyglot_disabled = ['latex','python','c','c++']
" }}}
" Others {{{

" let g:table_mode_corner="|"            " Table modeline
" let g:vimwiki_ext2syntax = {'.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'} " vimwiki markdown support

" }}}
"}}}
"{{{ ----------------- Color -----------------

set background=dark
if has('nvim')
    color solarized8
else
    color atom-dark-256
endif

"}}}
"}}}
" vim:foldmethod=marker foldlevel=0
