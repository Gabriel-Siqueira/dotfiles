setlocal expandtab
setlocal formatprg=hindent

nnoremap <silent> <leader>mi :<c-u>%!hindent<CR>
vnoremap <silent> <leader>mi :<c-u>'<,'>!hindent<CR>
