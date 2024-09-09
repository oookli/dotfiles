-- Define navigable file types
local navigable_filetypes = { 'vim', 'ruby', 'javascript', 'sh' }

-- Function to check if the current buffer is navigable (filetype and buffer type)
local function is_buffer_navigable()
  local current_filetype = vim.bo.filetype
  local current_buftype = vim.bo.buftype
  return vim.tbl_contains(navigable_filetypes, current_filetype) and current_buftype ~= 'nofile'
end

-- Function to enable tag navigation mappings in a buffer
local function enable_tag_nav_mappings()
  if is_buffer_navigable() then
    -- Mapping for tag navigation
    vim.api.nvim_buf_set_keymap(0, 'n', '<CR>', '<C-]>', { noremap = true, silent = true })
    vim.api.nvim_buf_set_keymap(0, 'n', '<BS>', '<C-t>', { noremap = true, silent = true })
  end
end

-- Autocommand to enable tag navigation mappings for all file types
vim.api.nvim_create_autocmd('FileType', {
  pattern = '*',
  callback = function()
    enable_tag_nav_mappings()
  end
})

-- Add tags file to search path
vim.opt.tags:append('./git/tags')
