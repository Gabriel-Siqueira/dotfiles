" Leader
let mapleader = "ç"

" NerdTree Ctrl n
map <C-n> :NERDTreeToggle<CR>

" Force to use h/j/k/l and arrows move line
nnoremap <left>  :echoe "use h"<cr>
nnoremap <right> :echoe "use l"<cr>
nnoremap <up> 	 :echoe "use j"<cr>
nnoremap <down>  :echoe "use k"<cr>

" Move a line of text using ALT+[jk]
nmap <M-j> mz:m+<cr>`z 
nmap <M-k> mz:m-2<cr>`z 
vmap <M-j> m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

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
