-- Neomake settings and makers configuration
vim.g.neomake_javascript_enabled_makers = { 'eslint' }
vim.g.neomake_jsx_enabled_makers = { 'eslint' }
vim.g.neomake_ruby_enabled_makers = { 'rubocop', 'mri' }
vim.g.neomake_haml_enabled_makers = { 'hamllint' }

vim.g.neomake_javascript_eslint_exe = 'eslint'
vim.g.neomake_ruby_rubocop_exe = 'bundle'
vim.g.neomake_ruby_rubocop_args = { 'exec', 'rubocop' }

-- Automatically run Neomake after file save
vim.cmd([[
  autocmd BufWritePost * Neomake
]])

-- Function to set Neomake ESLint path dynamically based on project structure
function NeomakeESlintChecker()
  for _, folder in ipairs({'', 'frontend', 'frontend_auto', 'frontend_home'}) do
    local eslint_path = vim.fn.getcwd() .. '/' .. folder .. '/node_modules/.bin/eslint'
    if vim.fn.executable(eslint_path) == 1 then
      vim.g.neomake_javascript_eslint_exe = eslint_path
      break
    end
  end
end

vim.cmd([[
  autocmd FileType javascript :lua NeomakeESlintChecker()
]])

-- Run Neomake on save for specific file types
vim.cmd([[
  autocmd BufWritePost *.js,*.jsx,*.rb,*.haml Neomake
]])
