vim.g.lsplist = {
    -- 'agda_ls',
    'clangd',
    'hls',
    'ltex',
    'lua_ls',
    'nil_ls',
    'pyright',
    'texlab'
}
-- require 'lspconfig'.agda_ls.setup {}
require 'lspconfig'.clangd.setup {}
require 'lspconfig'.hls.setup {}
require 'lspconfig'.ltex.setup { settings = { ltex = { language = "auto", }, } }
require 'lspconfig'.lua_ls.setup {}
require 'lspconfig'.nil_ls.setup {}
require 'lspconfig'.pyright.setup {}
require 'lspconfig'.texlab.setup {}
