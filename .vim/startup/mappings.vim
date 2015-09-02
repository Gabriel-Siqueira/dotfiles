" Leader
let mapleader = "@"

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
