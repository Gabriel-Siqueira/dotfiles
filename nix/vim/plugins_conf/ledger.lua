require("which-key").add({
  { "<leader>al",  group = "ledger" },
  { "<leader>ala", "<cmd>LedgerAlign<CR>",  desc = "aligned" },
  { "<leader>alb", "<cmd>Ledger bal<CR>",   desc = "balance" },
  { "<leader>all", "<cmd>e $MY_LEDGER<CR>", desc = "open ledger file" },
})
