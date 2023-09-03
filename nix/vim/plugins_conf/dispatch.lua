require("which-key").register({
  m = {
    name = "+make",
    m = { "<cmd>Make<CR>", "make" },
    c = { "<cmd>Make clean<CR>", "clean" },
  },
}, { prefix = "<leader>a" })
