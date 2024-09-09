-- Disable default EasyMotion mappings
vim.g.EasyMotion_do_mapping = 0

-- Set custom mapping to jump anywhere with EasyMotion using <Tab><Tab>
vim.api.nvim_set_keymap('n', '<Tab><Tab>', '<Plug>(easymotion-overwin-f)', {})

-- Enable smart case for EasyMotion
vim.g.EasyMotion_smartcase = 1
