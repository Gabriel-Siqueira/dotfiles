require 'hop'.setup()

local hop = require('hop')
local directions = require('hop.hint').HintDirection
vim.keymap.set('', 'h', function()
    hop.hint_char2({ direction = nil, current_line_only = false })
end, { remap = true })
vim.keymap.set('', 'l', function()
    hop.hint_lines({ direction = nil, current_line_only = false })
end, { remap = true })
