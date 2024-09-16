-- NERDTree key mappings
vim.api.nvim_set_keymap('n', '<Leader>nr', ':NERDTreeFind<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-a>', ':NERDTreeToggle<CR>', { noremap = true, silent = true })

-- NERDTree settings
vim.g.NERDTreeQuitOnOpen = 1  -- Automatically close NERDTree when a file is opened
vim.g.NERDTreeIgnore = { '\\.pyc$' }  -- Ignore .pyc files
-- vim.g.NERDTreeNodeDelimiter = "\u00a0"
vim.cmd([[
  let g:NERDTreeNodeDelimiter = "\u00a0"
]])  -- Avoid copying the ^G symbol when copying file paths
