-- Set leader key to space
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Save the file with <Leader><Leader>
vim.api.nvim_set_keymap('n', '<Leader><Leader>', ':w<CR>', { noremap = true, silent = true })

-- Quick search and replace. Global set to default
-- Do a / search first, then leave pattern empty in :s// to use previous
vim.api.nvim_set_keymap('n', '<Leader>sub', [[:%s///g<Left><Left>]], { noremap = true })
vim.api.nvim_set_keymap('v', '<Leader>sub', [[:s///g<Left><Left>]], { noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>wub', [[:%s//<C-r><C-w>/g<CR>]], { noremap = true })

-- Open and source vimrc quickly
vim.api.nvim_set_keymap('n', '<Leader>vr', ':sp $MYVIMRC<CR>', { noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>so', ':source $MYVIMRC<CR>', { noremap = true })

-- Window navigation
vim.api.nvim_set_keymap('n', '<C-k>', '<C-w>k', { noremap = true })
vim.api.nvim_set_keymap('n', '<C-j>', '<C-w>j', { noremap = true })
vim.api.nvim_set_keymap('n', '<C-l>', '<C-w>l', { noremap = true })
vim.api.nvim_set_keymap('n', '<C-h>', '<C-w>h', { noremap = true })

-- Toggle paste mode with F4
vim.api.nvim_set_keymap('n', '<F4>', ':set invpaste<CR>:set paste?<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', '<F4>', '<ESC>:set invpaste<CR>:set paste?<CR>', { noremap = true, silent = true })

-- Tab management
vim.api.nvim_set_keymap('n', '<Leader>wn', ':tabnew<CR>', { noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>ct', ':tabclose<CR>', { noremap = true })

-- Maximize split window
vim.api.nvim_set_keymap('n', '<Leader>-', ':wincmd _<CR>:wincmd |<CR>', { noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>=', ':wincmd =<CR>', { noremap = true })

-- Toggle search highlight
vim.api.nvim_set_keymap('n', '//', ':nohlsearch<CR>', { noremap = true, silent = true })

-- Move between quickfix items
vim.api.nvim_set_keymap('n', '<Right>', ':cnext<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Left>', ':cprev<CR>', { noremap = true, silent = true })

-- Easy access to formatting
vim.api.nvim_set_keymap('n', '<Leader>fef', ':normal! gg=G``<CR>', { noremap = true })

-- Easy access to splits
vim.api.nvim_set_keymap('n', '<Leader>vn', ':vsplit<CR>', { noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>vs', ':vsplit<CR>', { noremap = true })

-- Quickly clear the search highlighting
vim.api.nvim_set_keymap('n', '<Leader>hl', ':set hlsearch!<CR>', { noremap = true })

-- Resourcing vimrc
vim.api.nvim_set_keymap('n', '<Leader>sop', ':source %<CR>', { noremap = true })

-- Easy buffer delete
vim.api.nvim_set_keymap('n', '<Leader>x', ':bd<CR>', { noremap = true })

-- Move to the start of the line (remap '0' to '^')
vim.api.nvim_set_keymap('n', '0', '^', { noremap = true })

-- Yank till the end of the line (remap 'Y' to 'y$')
vim.api.nvim_set_keymap('n', 'Y', 'y$', { noremap = true })

-- Move cursor by visual lines
vim.api.nvim_set_keymap('n', 'j', 'gj', { noremap = true })
vim.api.nvim_set_keymap('n', 'k', 'gk', { noremap = true })

-- Reselect pasted text
vim.api.nvim_set_keymap('n', 'gp', '`[v`]', { noremap = true })

-- Go to the last edited position
vim.api.nvim_set_keymap('n', 'ge', '`.', { noremap = true })

-- Toggle list display (e.g., for trailing spaces)
vim.api.nvim_set_keymap('n', '<Leader>l', ':set list!<CR>', { noremap = true })

-- Insert new line in normal mode
vim.api.nvim_set_keymap('n', '<Leader>J', ':set paste<CR>m`o<Esc>``:set nopaste<CR>', { noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>K', ':set paste<CR>m`O<Esc>``:set nopaste<CR>', { noremap = true })

-- Tagbar toggle
vim.api.nvim_set_keymap('n', '<Leader>]', ':TagbarToggle<CR>', { noremap = true, silent = true })

-- C-Space to exit insert, visual, and other modes
vim.api.nvim_set_keymap('n', '<C-Space>', '<Esc>:noh<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<C-Space>', '<Esc>gV', { noremap = true, silent = true })
vim.api.nvim_set_keymap('o', '<C-Space>', '<Esc>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('c', '<C-Space>', '<C-c>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', '<C-Space>', '<Esc>', { noremap = true, silent = true })

-- C-Space (as Ctrl-@) in some terminal environments
vim.api.nvim_set_keymap('n', '<C-@>', '<Esc>:noh<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<C-@>', '<Esc>gV', { noremap = true, silent = true })
vim.api.nvim_set_keymap('o', '<C-@>', '<Esc>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('c', '<C-@>', '<C-c>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', '<C-@>', '<Esc>', { noremap = true, silent = true })

-- Alias :W to :write
vim.cmd('command! W write')

-- Easy navigation for editing and cding
vim.api.nvim_set_keymap('n', '<leader>R', ':e <C-r>=<CR>', { noremap = true, silent = true })

-- Make Y yank to end of line (like D)
vim.api.nvim_set_keymap('n', 'Y', 'y$', { noremap = true })

-- Easy access to the start of the line (first non-blank character)
vim.api.nvim_set_keymap('n', '0', '^', { noremap = true })

-- Go to the position of the last edit
vim.api.nvim_set_keymap('n', 'ge', '`.', { noremap = true })

-- Tab switching: <C-w> followed by a number switches to that tab
for tab_number = 1, 6 do
  vim.api.nvim_set_keymap('n', '<C-w>' .. tab_number, ':tabnext ' .. tab_number .. '<CR>', { noremap = true, silent = true })
end

-- Create a new tab
vim.api.nvim_set_keymap('n', '<leader>wn', ':tabnew<CR>', { noremap = true, silent = true })

-- Close the current tab
vim.api.nvim_set_keymap('n', '<leader>ct', ':tabclose<CR>', { noremap = true, silent = true })

-- Command-line mapping to save files with sudo when forgotten to open with sudo
vim.api.nvim_set_keymap('c', 'w!!', 'w !sudo tee > /dev/null %', { noremap = true, silent = true })

-- Session management
vim.api.nvim_set_keymap('n', '<F2>', ':mksession! ~/vim_session<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<F3>', ':source ~/vim_session<CR>', { noremap = true, silent = true })

-- Maximize window
vim.api.nvim_set_keymap('n', '<C-_>', '<C-w>_', { noremap = true, silent = true })

-- Rebalance and zoom windows
vim.api.nvim_set_keymap('n', '<leader>-', ':wincmd _<CR>:wincmd \\|<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>=', ':wincmd =<CR>', { noremap = true, silent = true })

-- Window movement mappings
vim.api.nvim_set_keymap('n', '<leader>mwh', '<C-w>H', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>mwj', '<C-w>J', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>mwk', '<C-w>K', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>mwl', '<C-w>L', { noremap = true, silent = true })

-- Mapping to call I18nTranslateString() in visual mode
vim.api.nvim_set_keymap('v', '<Leader>et', ':call I18nTranslateString()<CR>', { noremap = true, silent = true })

-- Map Backspace to TmuxNavigateLeft
vim.api.nvim_set_keymap('n', '<BS>', ':TmuxNavigateLeft<CR>', { noremap = true, silent = true })

-- Visual mode reverse insert
vim.api.nvim_set_keymap('v', ';rv', 'c<C-O>:set revins<CR><C-R>"<Esc>:set norevins<CR>', { noremap = true, silent = true })
