local luasnip = require("luasnip")

vim.keymap.set({"i"}, "<C-K>", function() luasnip.expand() end, {silent = true})
