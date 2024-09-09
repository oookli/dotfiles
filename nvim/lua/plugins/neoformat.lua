-- Neoformat configuration for auto-formatting
vim.g.neoformat_run_all_formatters = 1
vim.g.neoformat_only_msg_on_error = 1
vim.g.neoformat_try_node_exe = 1

-- Auto-formatting on save for specific file types
vim.cmd('autocmd BufWritePre *.js Neoformat')
vim.cmd('autocmd BufWritePre *.ts Neoformat')
vim.cmd('autocmd BufWritePre *.tsx Neoformat')
vim.cmd('autocmd BufWritePre *.scss Neoformat')

