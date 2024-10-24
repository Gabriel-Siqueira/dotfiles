vim.g.lsplist = {
    -- 'agda_ls',
    'ccls',
    'hls',
    'ltex',
    'lua_ls',
    'nil_ls',
    'pyright',
    'texlab'
}
-- require 'lspconfig'.agda_ls.setup {}
require 'lspconfig'.ccls.setup { compilationDatabaseDirectory = "build", index = { threads = 0, }, }
require 'lspconfig'.hls.setup { formattingProvider = "ourmolu", plugin = { ormolu = { config = { external = true, }, }, }, }
require 'lspconfig'.ltex.setup { settings = { ltex = { language = "auto", }, } }
require 'lspconfig'.lua_ls.setup {}
require 'lspconfig'.nil_ls.setup {}
require 'lspconfig'.pyright.setup {}
require 'lspconfig'.texlab.setup {}
