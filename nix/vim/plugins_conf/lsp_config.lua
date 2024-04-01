vim.g.lsplist = { 'hls', 'nil_ls', 'ltex', 'texlab', 'lua_ls', 'pyright', 'clangd' }
require 'lspconfig'.hls.setup {}
require 'lspconfig'.nil_ls.setup {}
require 'lspconfig'.ltex.setup {
    settings = {
        ltex = {
            language = "auto",
        },
    }
}
require 'lspconfig'.texlab.setup {}
require 'lspconfig'.lua_ls.setup {}
require 'lspconfig'.pyright.setup {}
require 'lspconfig'.clangd.setup {}
