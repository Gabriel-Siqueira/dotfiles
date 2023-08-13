require("which-key").register({
  m = {
    name = "+make",
    m = { ":Make", "make" },
    c = { ":Make clean", "clean" },
  },
}, { prefix = "<leader>a" })
