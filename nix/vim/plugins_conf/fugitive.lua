require("which-key").register({
  g = {
    name = "+git",
    s = { '<cmd>Git<CR>', 'status' },
    S = { '<cmd>Git add %<CR>', 'stage current file' },
    U = { '<cmd>Git reset -q %<CR>', 'unstage current file' },
    c = { '<cmd>Git commit<CR>', 'edit git commit' },
    p = { '<cmd>Git push<CR>', 'git push' },
    d = { '<cmd>Git diff<CR>', 'view git diff' },
    A = { '<cmd>Git add .<CR>', 'stage all files' },
    b = { '<cmd>Git blame<CR>', 'view git blame' },
    V = { '<cmd>Git log -- %<CR>', 'git log of current file' },
    v = { '<cmd>Git log --<CR>', 'git log of current repo' },
  },
}, { prefix = "<leader>" })
