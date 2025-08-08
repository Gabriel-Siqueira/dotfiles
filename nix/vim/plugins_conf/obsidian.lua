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
  config = {
    templates = {
      subdir = "Zettelkasten/Templates",
      date_format = "%Y-%m-%d",
      time_format = "%H:%M",
    },
  },
})

require("which-key").add({
  { "<leader>ao",  group = "obsidian" },
  { "<leader>aoS", "<cmd>Obsidian search<CR>",       desc = "search text in notes" },
  { "<leader>aob", "<cmd>Obsidian backlinks<CR>",    desc = "backlinks" },
  { "<leader>aof", "<cmd>Obsidian follow_link<CR>",  desc = "follow link" },
  { "<leader>aol", "<cmd>Obsidian link_new<CR>",     desc = "create a new link" },
  { "<leader>aon", "<cmd>Obsidian new<CR>",          desc = "new note" },
  { "<leader>aos", "<cmd>Obsidian quick_switch<CR>", desc = "search note name and open" },
  { "<leader>aot", "<cmd>Obsidian template<CR>",     desc = "add template" },
})
