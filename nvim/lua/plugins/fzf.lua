-- -- FZF key mappings
-- vim.api.nvim_set_keymap('n', '<C-p>', ':Files<CR>', { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', '<Leader>p', ':History<CR>', { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', '<Leader>\\', ':Buffers<CR>', { noremap = true, silent = true })

-- FZF file options (with preview)
vim.g.fzf_files_options = '--reverse --preview "(coderay {} || cat {}) 2> /dev/null | head -' .. vim.o.lines .. '"'

-- FZF buffers jump setting
vim.g.fzf_buffers_jump = 1

-- FZF history directory
vim.g.fzf_history_dir = '~/.local/share/fzf-history'

-- Set default command for FZF (ag with hidden files)
vim.env.FZF_DEFAULT_COMMAND = 'ag -g "" --hidden'

-- Define global function to close NERDTree if it's open
_G.close_nerdtree_if_open = function()
  local nerdtree_bufname = "NERD_tree_1"
  if vim.fn.bufwinnr(nerdtree_bufname) ~= -1 then
    vim.cmd('NERDTreeClose')
  end
end

-- Key mappings for FZF, close NERDTree if open first
vim.api.nvim_set_keymap('n', '<C-p>', '<Cmd>lua close_nerdtree_if_open(); vim.cmd("Files")<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>p', '<Cmd>lua close_nerdtree_if_open(); vim.cmd("History")<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>\\', '<Cmd>lua close_nerdtree_if_open(); vim.cmd("Buffers")<CR>', { noremap = true, silent = true })

-- Custom options for branch files and uncommitted files
local branch_files_options = { source = 'branch_files' }
local uncommited_files_options = { source = 'branch_files -w' }

local diff_options = '--reverse --preview "(mdiff {} | tail -n +5 || cat {}) 2> /dev/null | head -' .. vim.o.lines .. '"'

-- Commands for BranchFiles and UncommittedFiles
vim.api.nvim_create_user_command('BranchFiles', function()
    vim.fn['fzf#run'](vim.fn['fzf#wrap']('BranchFiles', vim.tbl_extend('keep', branch_files_options, { options = diff_options }), 0))
end, {})

vim.api.nvim_create_user_command('UncommitedFiles', function()
    vim.fn['fzf#run'](vim.fn['fzf#wrap']('UncommitedFiles', vim.tbl_extend('keep', uncommited_files_options, { options = diff_options }), 0))
end, {})

-- Key mappings for BranchFiles and UncommitedFiles
vim.api.nvim_set_keymap('n', '<leader>gp', ':BranchFiles<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>GP', ':UncommitedFiles<CR>', { noremap = true, silent = true })
