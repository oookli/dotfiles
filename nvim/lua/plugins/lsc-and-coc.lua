-- Coc.nvim mappings for language server functionality
vim.api.nvim_set_keymap('n', 'gd', '<Plug>(coc-definition)', { silent = true })
vim.api.nvim_set_keymap('n', 'gy', '<Plug>(coc-type-definition)', { silent = true })
vim.api.nvim_set_keymap('n', 'gi', '<Plug>(coc-implementation)', { silent = true })
vim.api.nvim_set_keymap('n', 'gr', '<Plug>(coc-references)', { silent = true })

-- Tab completion using Coc.nvim
vim.api.nvim_set_keymap('i', '<Tab>', 'pumvisible() ? coc#_select_confirm() : "<Tab>"', { expr = true, silent = true })

-- Set up basic LSP for JavaScript, Dart, and more using vim-lsc
vim.g.lsc_auto_map = true
