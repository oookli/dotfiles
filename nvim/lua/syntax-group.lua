function SyntaxGroup()
  local hl = vim.fn.synIDattr(vim.fn.synID(vim.fn.line("."), vim.fn.col("."), 1), "name")
  local trans = vim.fn.synIDattr(vim.fn.synID(vim.fn.line("."), vim.fn.col("."), 0), "name")
  local low = vim.fn.synIDattr(vim.fn.synIDtrans(vim.fn.synID(vim.fn.line("."), vim.fn.col("."), 1)), "name")
  print('hl<' .. hl .. '> trans<' .. trans .. '> low<' .. low .. '>')
end

vim.api.nvim_create_user_command('SyntaxGroupUnderCursor', 'lua SyntaxGroup()', {})
vim.api.nvim_set_keymap('n', '<LEADER>sg', ':SyntaxGroupUnderCursor<CR>', { noremap = true, silent = true })
