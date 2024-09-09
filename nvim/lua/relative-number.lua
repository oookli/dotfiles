-- Enable relative number by default
vim.opt.relativenumber = true

-- Enable absolute number on the current line (hybrid mode)
vim.opt.number = true

-- Function to toggle relative and absolute line numbers
function _G.NumberToggle()
    if vim.wo.relativenumber then
        vim.wo.relativenumber = false
        vim.wo.number = true
    else
        vim.wo.relativenumber = true
    end
end

-- Map F12 to toggle relative and absolute numbers
vim.api.nvim_set_keymap('n', '<F12>', ':lua NumberToggle()<CR>', { noremap = true, silent = true })
