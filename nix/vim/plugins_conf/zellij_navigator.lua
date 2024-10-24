require("zellij-nav").setup()

vim.keymap.set('n', '<C-Left>', '<cmd>ZellijNavigateLeft<cr>', { noremap = true, desc = 'move left (zellij aware)' })
vim.keymap.set('n', '<C-Down>', '<cmd>ZellijNavigateDown<cr>', { noremap = true, desc = 'move down (zellij aware)' })
vim.keymap.set('n', '<C-Up>', '<cmd>ZellijNavigateUp<cr>', { noremap = true, desc = 'move up (zellij aware)' })
vim.keymap.set('n', '<C-Right>', '<cmd>ZellijNavigateRight<cr>', { noremap = true, desc = 'move right (zellij aware)' })
