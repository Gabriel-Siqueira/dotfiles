require("which-key").add({
  { "<leader>g",  group = "git" },
  { "<leader>gA", "<cmd>Git add .<CR>",      desc = "stage all files" },
  { "<leader>gS", "<cmd>Git add %<CR>",      desc = "stage current file" },
  { "<leader>gU", "<cmd>Git reset -q %<CR>", desc = "unstage current file" },
  { "<leader>gV", "<cmd>Git log -- %<CR>",   desc = "git log of current file" },
  { "<leader>gb", "<cmd>Git blame<CR>",      desc = "view git blame" },
  { "<leader>gc", "<cmd>Git commit<CR>",     desc = "edit git commit" },
  { "<leader>gd", "<cmd>Git diff<CR>",       desc = "view git diff" },
  { "<leader>gp", "<cmd>Git push<CR>",       desc = "git push" },
  { "<leader>gs", "<cmd>Git<CR>",            desc = "status" },
  { "<leader>gv", "<cmd>Git log --<CR>",     desc = "git log of current repo" },
})
