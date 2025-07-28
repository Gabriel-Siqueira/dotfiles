require("obsidian").setup({
  workspaces = {
  {
    name = "main",
    path = "$MY_OBSIDIAN",
  },
  },
  notes_subdir = "Zettelkasten/Notes",
  legacy_commands = false,
  disable_frontmatter = true,
  templates = {
    subdir = "Zettelkasten/Templates",
    date_format = "%Y-%m-%d",
    time_format = "%H:%M",
  },
})

require("which-key").add({
  { "<leader>ao",  group = "obsidian" },
  { "<leader>aoS", "<cmd>ObsidianSearch<CR>",      desc = "search text in notes" },
  { "<leader>aob", "<cmd>ObsidianBacklinks<CR>",   desc = "backlinks" },
  { "<leader>aof", "<cmd>ObsidianFollowLink<CR>",  desc = "follow link" },
  { "<leader>aol", "<cmd>ObsidianLinkNew<CR>",     desc = "create a new link" },
  { "<leader>aon", "<cmd>ObsidianNew<CR>",         desc = "new note" },
  { "<leader>aos", "<cmd>ObsidianQuickSwitch<CR>", desc = "search note name and open" },
  { "<leader>aot", "<cmd>ObsidianTemplate<CR>",    desc = "add template" },
})
