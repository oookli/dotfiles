-- Automatically remove trailing whitespace
vim.cmd('autocmd BufWritePre * :%s/\\s\\+$//e')

-- Automatically rebalance windows when resizing
vim.cmd('autocmd VimResized * :wincmd =')
