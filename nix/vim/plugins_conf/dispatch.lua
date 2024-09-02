require("which-key").add({
  { "<leader>am",  group = "make" },
  { "<leader>amc", "<cmd>Make clean<CR>", desc = "clean" },
  { "<leader>amm", "<cmd>Make<CR>",       desc = "make" },
})
