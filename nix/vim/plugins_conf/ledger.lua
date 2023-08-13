require("which-key").register({
  l = {
    name = "+ledger",
    l = { "<cmd>e $MY_LEDGER<CR>", "open ledger file" },
    b = { "<cmd>Ledger bal<CR>", "balance" },
    a = { "<cmd>LedgerAlign<CR>", "aligned" },
  },
}, { prefix = "<leader>a" })
