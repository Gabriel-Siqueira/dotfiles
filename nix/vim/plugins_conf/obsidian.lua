require("obsidian").setup({
  dir = "$MY_OBSIDIAN",
  notes_subdir = "Zettelkasten/Notes",
  mappings = {},
  disable_frontmatter = true,
  templates = {
    subdir = "Zettelkasten/Templates",
    date_format = "%Y-%m-%d",
    time_format = "%H:%M",
  },
})

require("which-key").register({
  o = {
    name = "+obsidian",
    s = { "<cmd>ObsidianQuickSwitch<CR>", "search note name and open" },
    S = { "<cmd>ObsidianSearch<CR>", "search text in notes" },
    n = { "<cmd>ObsidianNew<CR>", "new note" },
    b = { "<cmd>ObsidianBacklinks<CR>", "backlinks" },
    l = { "<cmd>ObsidianLinkNew<CR>", "create a new link" },
    f = { "<cmd>ObsidianFollowLink<CR>", "follow link" },
    t = { "<cmd>ObsidianTemplate<CR>", "add template" },
  },
}, { prefix = "<leader>a" })
