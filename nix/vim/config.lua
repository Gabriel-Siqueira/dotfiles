-- {{{ ====================== Functions ======================

local switch_spell_lang = function()
	-- Loop through languages.
	vim.opt.spelllang = vim.g.myLangList[vim.g.myLang]
	print('language:' .. vim.g.myLangList[vim.g.myLang])
	vim.g.myLang = vim.g.myLang + 1
	if vim.g.myLang > #vim.g.myLangList then vim.g.myLang = 1 end
end

local open_bib = function(how)
	-- Open file associated with reference under the cursor.
	local save_reg = vim.fn.getreg("@")
	vim.fn.setreg("@", "failure")
	local cite = nil
	vim.cmd("normal! yi[")
	if vim.fn.getreg("@") == "failure" then
		vim.cmd("normal! yi{")
		if vim.fn.getreg("@") == "failure" then
			vim.cmd("normal! yi{")
			cite = vim.split(vim.fn.getreg("@"), ",")[1]
		else
			cite = vim.fn.getreg("@")
		end
	else
		cite = vim.split(vim.fn.getreg("@"), "@")[2]
	end
	if how == "pdf" then
		local file = vim.env.MY_REFS .. cite .. ".pdf"
		print(file)
		vim.fn.system("xdg-open " .. file .. "&")
	elseif how == "bib" then
		vim.cmd("e $MY_BIB")
		vim.fn.search(cite)
	else
		print('Error: Option non existent!')
	end
	vim.fn.setreg("@", save_reg)
end

local open_bib_pdf = function()
	open_bib("pdf")
end

local open_bib_bib = function()
	open_bib("bib")
end

local set_num = function(num)
	-- Set number in number register.
	vim.g.myNum = num
end

local next_num = function()
	-- Replace character with number of number register and increment number
	-- register.
	local save_reg = vim.fn.getreg("n")
	vim.fn.setreg("@", tostring(vim.g.myNum))
	execute("normal! diw\"np")
	vim.g.myNum = vim.g.myNum + 1
	vim.fn.setreg("@", save_reg)
end

local show_documentation = function()
	-- Show documentation.
	if vim.bo.filetype == "vim" or vim.bo.filetype == "help" then
		vim.cmd("h " .. vim.fn.expand("<cword>"))
	else
		vim.lsp.buf.hover()
	end
end

local open_pdf = function()
	-- Open pdf file.
	local pdf = vim.fn.expand("%:p:r") .. ".pdf"
	local pdfs = vim.fn.glob("*.pdf", 0, 1)
	if vim.fn.filereadable(pdf) then
		vim.fn.system("xdg-open " .. pdf .. "&")
	elseif not vim.fn.empty(pdfs) then
		vim.fn.system("xdg-open " .. pdfs[1] .. "&")
	else
		print("No pdf available")
	end
end

local send_line = function()
	-- Send line to tmux pane.
	vim.fn.system("tmux send-keys -t 0 " .. vim.fn.getline(".") .. " Enter")
	vim.cmd("normal! <Plug>(ripple_send_line)")
	vim.cmd("normal! j")
end

-- }}}
-- {{{ ====================== Mappings =======================

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

local wk = require("which-key")

vim.keymap.set('n', '<leader><Tab>', '<cmd>b#<CR>', { noremap = true, desc = 'go to previous buffer' })

wk.register({
  a = {
    name = "+app",
  },
}, { prefix = "<leader>" })

wk.register({
  b = {
    name = "+buffer",
    d = { '<cmd>bd<CR>', 'delete buffer' },
    n = { '<cmd>bn<CR>', 'next buffer' },
    p = { '<cmd>bp<CR>', 'prev buffer' },
  },
}, { prefix = "<leader>" })

wk.register({
  b = {
    name = "+buffer",
    d = { '<cmd>bd<CR>', 'delete buffer' },
    n = { '<cmd>bn<CR>', 'next buffer' },
    p = { '<cmd>bp<CR>', 'prev buffer' },
  },
}, { prefix = "<leader>" })

wk.register({
  e = {
    name = "+errors",
    l = { vim.diagnostic.show, 'list' },
    n = { vim.diagnostic.goto_next, 'next' },
    p = { vim.diagnostic.goto_prev, 'prev' },
  },
}, { prefix = "<leader>" })

wk.register({
  f = {
    name = "+file",
    s = { '<cmd>update<CR>', 'save' },
    f = { '<cmd>Telescope find_files<CR>', 'find file' },
  },
}, { prefix = "<leader>" })

wk.register({
  h = {
    name = "+help",
  },
}, { prefix = "<leader>" })

wk.register({
  l = {
    name = "+language",
    r = { vim.lsp.buf.rename, 'rename' },
    f = { vim.lsp.buf.format, 'format' },
    a = { vim.lsp.buf.code_action, 'action' },
    d = { vim.lsp.buf.hover, 'describe' },
  },
}, { prefix = "<leader>" })

wk.register({
  a = {
    name = "+insert",
  },
}, { prefix = "<leader>" })

wk.register({
  j = {
    name = "+jump",
    d = { vim.lsp.buf.definition, 'definition' },
    i = { vim.lsp.buf.implementation, 'implementation' },
  },
}, { prefix = "<leader>l" })


vim.keymap.set('n', 'K', vim.lsp.buf.hover, { noremap = true, desc = 'hover' })

wk.register({
  n = {
    name = "+numbers",
    s = { set_num, 'set' },
    n = { next_num, 'next' },
  },
}, { prefix = "<leader>" })

wk.register({
  o = {
    name = "+open",
    p = { open_pdf, 'pdf' },
    w = { '<cmd>silent !firefox %<CR>', 'firefox' },
  },
}, { prefix = "<leader>" })

wk.register({
  c = {
    name = "+citation",
    p = { open_bib_pdf, 'pdf' },
    c = { open_bib_bib, 'bib' },
  },
}, { prefix = "<leader>o" })

wk.register({
  s = {
    name = "+search",
    c = { '<cmd>noh<CR>', 'clear' },
  },
}, { prefix = "<leader>" })

wk.register({
  S = {
    name = "+syntax",
    c = { switch_spell_lang, 'change dictionary' },
    s = { '<cmd>setlocal spell!<CR>', 'toggle spell check' },
  },
}, { prefix = "<leader>" })

wk.register({
  t = {
    name = "+tabs",
    n = { '<cmd>tabnew<CR>', 'new tab' },
    o = { '<cmd>tabonly<CR>', 'single tab' },
    c = { '<cmd>tabclose<CR>', 'close tab' },
    m = { '<cmd>tabmove<CR>', 'move tab' },
  },
}, { prefix = "<leader>" })

wk.register({
  w = {
    name = "+windows",
    w = { '<C-W>w', 'other window' },
    d = { '<C-W>c', 'delete window' },
    ['<Left>'] = { '<C-W>h', 'window left' },
    ['<Down>'] = { '<C-W>j', 'window below' },
    ['<Right>'] = { '<C-W>l', 'window right' },
    ['<Up>'] = { '<C-W>k', 'window up' },
    ['<'] = { '<C-W>5<', 'expand window left' },
    ['>'] = { '<C-W>5>', 'expand window right' },
    ['-'] = { '<cmd>resize -5<CR>', 'expand window up' },
    ['+'] = { '<cmd>resize +5<CR>', 'expand window down' },
    ['='] = { '<C-W>=', 'balance window' },
    s = { '<C-W>s', 'split window below' },
    v = { '<C-W>v', 'split window right' },
  },
}, { prefix = "<leader>" })

wk.register({
  w = {
    name = "+move",
    ['<Left>'] = { '<C-W>H', 'move window left' },
    ['<Down>'] = { '<C-W>J', 'move window below' },
    ['<Right>'] = { '<C-W>L', 'move window right' },
    ['<Up>'] = { '<C-W>K', 'move window up' },
  },
}, { prefix = "<leader>w" })

-- }}}
-- {{{ ====================== Settings =======================

-- misc {{{

vim.o.backspace='indent,eol,start' -- make backspace work
vim.o.shiftwidth=4     -- size off >>, << and ==
vim.o.tabstop=4        -- size of a <tab>
vim.o.expandtab=true   -- use spaces instead of tab
vim.o.autoindent=true  -- automatic insert indentation
vim.o.number=true      -- Line number
vim.o.numberwidth=2    -- size of numbers
vim.o.cursorline=true  -- Highlight cursor line
vim.o.textwidth=0      -- Do not break lines
if vim.fn.has('linebreak') == 1 then
	vim.o.wrap=true        -- make virtual lines
	vim.o.linebreak=true   -- wrap long lines at characters in 'breakat'
	vim.o.showbreak='⤷ '  -- character for line break (U+2937, UTF-8: E2 A4 B7)
end
vim.o.mouse='a'          -- enable mouse
vim.o.mousemodel='popup' -- right button open pupup menu
vim.o.scrolloff=10     -- Keep cursor centered
vim.o.sidescrolloff=3  -- scrolloff for columns
vim.o.showmatch=true   -- Show matching brackets when text indicator is over them
vim.o.hidden=true      -- Do not need to save buffers to open others
if vim.fn.has('termguicolors') == 1 then
	vim.o.termguicolors=true -- enable 24-bit RGB colors
end
vim.o.infercase=true   -- when using upper case complete only with upper case
vim.o.encoding='utf8'  -- Set utf8 as standard encoding
if vim.fn.exists('&inccommand') then
	vim.o.inccommand=split -- split in incremental search
end
vim.o.redrawtime=500   -- time limit to highlight search
vim.o.autoread=true    -- Set to auto read when a file is changed from the outside
vim.o.splitright=true  -- split on the right
vim.o.showmode=false   -- remove text insert from command line (status line already have it)
vim.o.virtualedit='block' -- better block selection
vim.o.fileformats='unix,dos,mac' -- order of file format
vim.o.laststatus=3      -- Always display the status line
vim.o.lazyredraw=true   -- Don't redraw while executing macros (good performance)
vim.o.list=true         -- show blanc chars
vim.opt.listchars = {
  tab = '» ',
  trail = '-',
  extends = '>',
  precedes = '<',
  eol = '¬',
  nbsp = '·',
  space = ' ',
}
vim.o.omnifunc='syntaxcomplete#Complete' -- omnicompletion
vim.o.showcmd=true      -- Show current commands
if vim.fn.has('wildmenu') == 1 then
	vim.o.wildmenu=true   -- show options as list when switching buffers etc
end
vim.o.wildmode='longest:full,full' -- shell-like autocomplete to unambiguous portion
if vim.fn.has('windows') == 1 then
	vim.o.fillchars='vert:┃'
end
vim.o.cmdheight=2        -- give more space for displaying messages
vim.o.foldmethod='expr'  -- fold using expressions
vim.o.signcolumn='yes'   -- display signs with the numbers
vim.o.spell=true         -- spell check on by defaut

-- List of languages to toggle between
vim.g.myLang = 1
vim.g.myLangList = {'pt_br', 'en_us', 'fr'}

-- set backup           -- keep backup file after write
vim.o.undofile=true     -- use undo files

-- don't create root-owned files
if vim.fn.exists('$SUDO_USER') == 1 then
	if vim.fn.has('persistent_undo') == 1 then
		vim.o.undofile=false
	end
	vim.o.swapfile=false
	vim.o.backup=false
	vim.o.writebackup=false
end

-- }}}
-- languages/file types {{{

-- latex
vim.g.tex_flavor = 'latex'

-- nix
vim.api.nvim_create_autocmd('BufEnter', {
	pattern = '*.nix',
	command = 'setlocal commentstring=#%s'
})

-- }}}

-- {{{ other autocommands

-- When editing a file, always jump to the last known cursor position.
vim.api.nvim_create_autocmd('BufRead', {
  callback = function(opts)
    vim.api.nvim_create_autocmd('BufWinEnter', {
      once = true,
      buffer = opts.buf,
      callback = function()
        local ft = vim.bo[opts.buf].filetype
        local last_known_line = vim.api.nvim_buf_get_mark(opts.buf, '"')[1]
        if
          not (ft:match('commit') and ft:match('rebase'))
          and last_known_line > 1
          and last_known_line <= vim.api.nvim_buf_line_count(opts.buf)
        then
          vim.api.nvim_feedkeys([[g`"]], 'nx', false)
        end
      end,
    })
  end,
})

-- }}}

-- }}}
-- vim: foldmethod=marker foldlevel=0
