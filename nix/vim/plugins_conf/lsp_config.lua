vim.g.lsplist = {'hls','rnix','ltex','texlab','lua_ls'}
require'lspconfig'.hls.setup{
    capabilities = capabilities
}
require'lspconfig'.rnix.setup{
    capabilities = capabilities
}
require'lspconfig'.ltex.setup{}
require'lspconfig'.texlab.setup{}
require'lspconfig'.lua_ls.setup{}
