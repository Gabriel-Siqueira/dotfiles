-- Read API key from file
local function read_api_key_from_file(file_path)
  local file = io.open(file_path, "r")
  if not file then
    return nil
  end
  local api_key = file:read("*l") -- read first line
  file:close()
  return api_key and api_key:gsub("^%s*(.-)%s*$", "%1") -- trim whitespace
end

-- Get API key
local api_key = read_api_key_from_file(vim.fn.expand("$DROPBOX/Backup/pc/languageToolKey.txt"))

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
require 'lspconfig'.ltex.setup {
    settings = {
        ltex = {
            language = "auto",
            languageToolHttpServerUri='https://api.languagetoolplus.com',
            languageToolOrg = {
                username='gabriel.gabrielhs@gmail.com',
                apiKey = api_key,
            }
        },
    }
}
require 'lspconfig'.lua_ls.setup {}
require 'lspconfig'.nil_ls.setup {}
require 'lspconfig'.pyright.setup {}
require 'lspconfig'.ruff.setup {}
require 'lspconfig'.texlab.setup {
    settings = {
        latex = {
            forwardSearch = {
                executable = "okular",
                args = { "--unique", "file:%p#src:%l%f" },
                onSave = true,
            }
        }
    }
}
