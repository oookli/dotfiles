-- Tmux-runner spec runner settings
vim.g.spec_runner_dispatcher = "VtrSendCommand! {command}"
vim.g.VtrOrientation = "h"  -- Set tmux window split orientation to horizontal

-- Key mapping for running focused spec
vim.api.nvim_set_keymap('n', '<Leader>t', '<Plug>RunFocusedSpec', { noremap = true, silent = true })
