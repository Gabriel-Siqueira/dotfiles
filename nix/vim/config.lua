-- {{{ ====================== Functions ======================

local switch_spell_lang = function()
	-- Loop through languages.
	vim.opt.spelllang = vim.g.myLangList[vim.g.myLang]
	local new_language = string.gsub(vim.g.myLangList[vim.g.myLang],"_","-")
	local clients = vim.lsp.get_active_clients()
    for _, client in ipairs(clients) do
        if client.name == "ltex" then
            client.config.settings.ltex.language = new_language
        end
    end
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

wk.add({
	{ "<leader>a", group = "app" },
})

wk.add({
	{ "<leader>b",  group = "buffer" },
	{ "<leader>bd", "<cmd>bd<CR>",   desc = "delete buffer" },
	{ "<leader>bn", "<cmd>bn<CR>",   desc = "next buffer" },
	{ "<leader>bp", "<cmd>bp<CR>",   desc = "prev buffer" },
})

wk.add({
	{ "<leader>e",  group = "errors" },
	{ "<leader>el", vim.diagnostic.show,       desc = "list" },
	{ "<leader>en", vim.diagnostic.goto_next,  desc = "next" },
	{ "<leader>ep", vim.diagnostic.goto_prev,  desc = "prev" },
	{ "<leader>es", vim.diagnostic.open_float, desc = "show" },
})

wk.add({
	{ "<leader>f",  group = "file" },
	{ "<leader>ff", "<cmd>Telescope find_files<CR>", desc = "find file" },
	{ "<leader>fs", "<cmd>update<CR>",               desc = "save" },
})

wk.add({
	{ "<leader>h", group = "help" },
})

wk.add({
	{ "<leader>l",  group = "language" },
	{ "<leader>la", vim.lsp.buf.code_action, desc = "action" },
	{ "<leader>ld", vim.lsp.buf.hover,       desc = "describe" },
	{ "<leader>lf", vim.lsp.buf.format,      desc = "format" },
	{ "<leader>lr", vim.lsp.buf.rename,      desc = "rename" },
	{ "<leader>lh", vim.lsp.buf.hover,       desc = "hover" },
})

wk.add({
	{ "<leader>i", group = "insert" },
})

wk.add({
	{ "<leader>lj",  group = "jump" },
	{ "<leader>ljd", vim.lsp.buf.definition,     desc = "definition" },
	{ "<leader>lji", vim.lsp.buf.implementation, desc = "implementation" },
})


vim.keymap.set('n', 'K', vim.lsp.buf.hover, { noremap = true, desc = 'hover' })

wk.add({
	{ "<leader>n",  group = "numbers" },
	{ "<leader>nn", next_num,         desc = "next" },
	{ "<leader>ns", set_num,          desc = "set" },
})

wk.add({
	{ "<leader>o",  group = "open" },
	{ "<leader>op", open_pdf,                     desc = "pdf" },
	{ "<leader>ow", "<cmd>silent !firefox %<CR>", desc = "firefox" },
})

wk.add({
	{ "<leader>oc",  group = "citation" },
	{ "<leader>occ", open_bib_bib,      desc = "bib" },
	{ "<leader>ocp", open_bib_pdf,      desc = "pdf" },
})

wk.add({
	{ "<leader>s",  group = "search" },
	{ "<leader>sc", "<cmd>noh<CR>",  desc = "clear" },
})

wk.add({
	{ "<leader>S",  group = "syntax" },
	{ "<leader>Sc", switch_spell_lang,          desc = "change dictionary" },
	{ "<leader>Ss", "<cmd>setlocal spell!<CR>", desc = "toggle spell check" },
})

wk.add({
	{ "<leader>t",  group = "tabs" },
	{ "<leader>tc", "<cmd>tabclose<CR>", desc = "close tab" },
	{ "<leader>tm", "<cmd>tabmove<CR>",  desc = "move tab" },
	{ "<leader>tn", "<cmd>tabnew<CR>",   desc = "new tab" },
	{ "<leader>to", "<cmd>tabonly<CR>",  desc = "single tab" },
})

wk.add({
	{ "<leader>w",        group = "windows" },
	{ "<leader>w+",       "<cmd>resize +5<CR>", desc = "expand window down" },
	{ "<leader>w-",       "<cmd>resize -5<CR>", desc = "expand window up" },
	{ "<leader>w<",       "<C-W>5<",            desc = "expand window left" },
	{ "<leader>w<Down>",  "<C-W>j",             desc = "window below" },
	{ "<leader>w<Left>",  "<C-W>h",             desc = "window left" },
	{ "<leader>w<Right>", "<C-W>l",             desc = "window right" },
	{ "<leader>w<Up>",    "<C-W>k",             desc = "window up" },
	{ "<leader>w=",       "<C-W>=",             desc = "balance window" },
	{ "<leader>w>",       "<C-W>5>",            desc = "expand window right" },
	{ "<leader>wd",       "<C-W>c",             desc = "delete window" },
	{ "<leader>ws",       "<C-W>s",             desc = "split window below" },
	{ "<leader>wv",       "<C-W>v",             desc = "split window right" },
	{ "<leader>w<Tab>",   "<C-W>w",             desc = "other window" },
})

wk.add({
	{ "<leader>ww",        group = "move" },
	{ "<leader>ww<Down>",  "<C-W>J",      desc = "move window below" },
	{ "<leader>ww<Left>",  "<C-W>H",      desc = "move window left" },
	{ "<leader>ww<Right>", "<C-W>L",      desc = "move window right" },
	{ "<leader>ww<Up>",    "<C-W>K",      desc = "move window up" },
})

-- }}}
-- {{{ ====================== Settings =======================

-- misc {{{

vim.o.backspace = 'indent,eol,start' -- make backspace work
vim.o.shiftwidth = 4 -- size off >>, << and ==
vim.o.tabstop = 4 -- size of a <tab>
vim.o.expandtab = true -- use spaces instead of tab
vim.o.autoindent = true -- automatic insert indentation
vim.o.number = true -- Line number
vim.o.numberwidth = 2 -- size of numbers
vim.o.cursorline = true -- Highlight cursor line
vim.o.textwidth = 0 -- Do not break lines
if vim.fn.has('linebreak') == 1 then
	vim.o.wrap = true -- make virtual lines
	vim.o.linebreak = true -- wrap long lines at characters in 'breakat'
	vim.o.showbreak = '⤷ ' -- character for line break (U+2937, UTF-8: E2 A4 B7)
end
vim.o.mouse = 'a' -- enable mouse
vim.o.mousemodel = 'popup' -- right button open pupup menu
vim.o.scrolloff = 10 -- Keep cursor centered
vim.o.sidescrolloff = 3 -- scrolloff for columns
vim.o.showmatch = true -- Show matching brackets when text indicator is over them
vim.o.hidden = true -- Do not need to save buffers to open others
if vim.fn.has('termguicolors') == 1 then
	vim.o.termguicolors = true -- enable 24-bit RGB colors
end
vim.o.infercase = true -- when using upper case complete only with upper case
vim.o.encoding = 'utf8' -- Set utf8 as standard encoding
if vim.fn.exists('&inccommand') then
	vim.o.inccommand = split -- split in incremental search
end
vim.o.redrawtime = 500 -- time limit to highlight search
vim.o.autoread = true -- Set to auto read when a file is changed from the outside
vim.o.splitright = true -- split on the right
vim.o.showmode = false -- remove text insert from command line (status line already have it)
vim.o.virtualedit = 'block' -- better block selection
vim.o.fileformats = 'unix,dos,mac' -- order of file format
vim.o.laststatus = 3 -- Always display the status line
vim.o.lazyredraw = true -- Don't redraw while executing macros (good performance)
vim.o.list = true -- show blanc chars
vim.opt.listchars = {
	tab = '» ',
	trail = '-',
	extends = '>',
	precedes = '<',
	eol = '¬',
	nbsp = '·',
	space = ' ',
}
vim.o.omnifunc = 'syntaxcomplete#Complete' -- omnicompletion
vim.o.showcmd = true                       -- Show current commands
if vim.fn.has('wildmenu') == 1 then
	vim.o.wildmenu = true                  -- show options as list when switching buffers etc
end
vim.o.wildmode = 'longest:full,full'       -- shell-like autocomplete to unambiguous portion
if vim.fn.has('windows') == 1 then
	vim.o.fillchars = 'vert:┃'
end
vim.o.cmdheight = 2       -- give more space for displaying messages
vim.o.foldmethod = 'expr' -- fold using expressions
vim.o.signcolumn = 'yes'  -- display signs with the numbers
vim.o.spell = true        -- spell check on by defaut

-- List of languages to toggle between
vim.g.myLang = 1
vim.g.myLangList = { 'pt_br', 'en_us', 'fr' }

-- set backup           -- keep backup file after write
vim.o.undofile = true -- use undo files

-- don't create root-owned files
if vim.fn.exists('$SUDO_USER') == 1 then
	if vim.fn.has('persistent_undo') == 1 then
		vim.o.undofile = false
	end
	vim.o.swapfile = false
	vim.o.backup = false
	vim.o.writebackup = false
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
