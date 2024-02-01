require("other-nvim").setup({
    mappings = {
        -- builtin mappings
        "livewire",
        "angular",
        "laravel",
        "rails",
        "golang",
        -- custom mapping
        {
            context = "test",
            pattern = "src/(.*).hs$",
            target = "test/%1Check.hs",
        },
        {
            context = "implementation",
            pattern = "test/(.*)Check.hs$",
            target = "src/%1.hs",
        },
        {
            context = "test",
            pattern = "src/(.*)/(.*).hs$",
            target = "test/%1/%2Check.hs",
        },
        {
            context = "implementation",
            pattern = "test/(.*)/(.*)Check.hs$",
            target = "src/%1/%2.hs",
        }
    },
    transformers = {
        -- defining a custom transformer
    },
    style = {
        -- How the plugin paints its window borders
        -- Allowed values are none, single, double, rounded, solid and shadow
        border = "solid",

        -- Column seperator for the window
        seperator = "|",

	-- width of the window in percent. e.g. 0.5 is 50%, 1.0 is 100%
	width = 0.7,

	-- min height in rows.
	-- when more columns are needed this value is extended automatically
	minHeight = 2
    },
})

vim.keymap.set('n', '<leader>fa', '<cmd>Other<CR>', { noremap = true, desc = 'alternate file' })
