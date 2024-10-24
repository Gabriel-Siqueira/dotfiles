require("telescope").setup({
    extensions = {
        undo = {},
        bibtex = {
            context = true,
            context_fallback = true,
            global_files = {"$MY_BIB"},
        },
    },
})
require("telescope").load_extension("undo")
require("telescope").load_extension("bibtex")
-- require("telescope").load_extension("noice")

vim.keymap.set('n', '<leader>bb', '<cmd>Telescope buffers<CR>', { noremap = true, desc = 'select buffer' })
vim.keymap.set('n', '<leader>hv', '<cmd>Telescope help_tags<CR>', { noremap = true, desc = 'help' })
vim.keymap.set('n', "<leader>'", '<cmd>Telescope marks<CR>', { noremap = true, desc = 'marks' })
vim.keymap.set('n', '<leader>"', '<cmd>Telescope marks<CR>', { noremap = true, desc = 'registers' })
vim.keymap.set('n', '<leader>ljr', '<cmd>Telescope lsp_references<CR>', { noremap = true, desc = 'references' })
vim.keymap.set('n', '<leader>ss', '<cmd>Telescope live_grep<CR>', { noremap = true, desc = 'grep' })
vim.keymap.set('n', '<leader>au', '<cmd>Telescope undo<CR>', { noremap = true, desc = 'unde tree' })
vim.keymap.set('n', '<leader>ic', '<cmd>Telescope bibtex<CR>', { noremap = true, desc = 'citation' })
