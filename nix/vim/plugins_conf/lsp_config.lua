-- Read API key from file
local function read_api_key_from_file(file_path)
    local file = io.open(file_path, "r")
    if not file then
        return nil
    end
    local api_key = file:read("*l")                       -- read first line
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
    'ruff',
    'texlab'
}

-- vim.lsp.config["agda_ls"] = {}
vim.lsp.config["ccls"] = {
    compilationDatabaseDirectory = "build",
    index = { threads = 0, },
}
vim.lsp.config["hls"] = {
    formattingProvider = "ourmolu",
    plugin = { ormolu = { config = { external = true, }, }, },
}
vim.lsp.config["ltex"] = {
    settings = {
        ltex = {
            language = "auto",
            languageToolHttpServerUri = 'https://api.languagetoolplus.com',
            languageToolOrg = {
                username = 'gabriel.gabrielhs@gmail.com',
                apiKey = api_key,
            }
        },
    }
}
vim.lsp.config["lua_ls"] = {}
vim.lsp.config["nil_ls"] = {}
vim.lsp.config["pyright"] = {}
vim.lsp.config["ruff"] = {}
vim.lsp.config["texlab"] = {
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

for _, v in ipairs(vim.g.lsplist) do
    vim.lsp.enable(v);
end
