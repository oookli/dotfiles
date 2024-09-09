-- CtrlSF key mappings
vim.api.nvim_set_keymap('n', '<Leader>,', ':CtrlSF<Space>', { noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>;', ':CtrlSF -R -I<Space>', { noremap = true })

-- CtrlSF settings
vim.g.ctrlsf_extra_backend_args = {
  ag = '--ignore node_modules --ignore dist --ignore build'
}
