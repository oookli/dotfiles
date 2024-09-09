-- General settings
vim.opt.hidden = true -- Allow buffer change w/o saving
vim.opt.autoread = true -- Load file from disk, ie for git reset
vim.opt.lazyredraw = true -- Don't update while executing macros
vim.opt.backspace = 'indent,eol,start' -- Sane backspace behavior
vim.opt.history = 1000 -- Remember last 1000 commands
vim.opt.scrolloff = 4 -- Keep at least 4 lines below cursor
vim.opt.expandtab = true -- Convert <tab> to spaces (2 or 4)
vim.opt.tabstop = 2 -- Two spaces per tab as default
vim.opt.shiftwidth = 2 -- then override with per filteype
vim.opt.softtabstop = 2 -- specific settings via autocmd
vim.opt.secure = true -- Limit what modelines and autocmds can do
vim.opt.autowrite = true -- write for me when I take any action

-- Set shortmess to avoid intro message
vim.opt.shortmess:append('I')

-- Enable file type detection, plugins, and indentation
vim.cmd('filetype plugin indent on')

-- Disable swap files
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false

-- Persistent undo
local undodir = vim.fn.expand('~/.undo-vim')
if not vim.fn.isdirectory(undodir) then
    vim.fn.mkdir(undodir)
end
vim.opt.undodir = undodir
vim.opt.undofile = true

-- Search settings
vim.opt.hlsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.wrap = true
vim.opt.linebreak = true
vim.opt.list = true
vim.opt.listchars = { tab = '>·', eol = '¬', trail = '·' }

-- Line-breaking settings

-- Set the showbreak symbol to '↳ ' for wrapped lines
vim.opt.showbreak = '↳ '
-- Enable breakindent for visually indented wrapped lines
vim.opt.breakindent = true
-- Set breakindent options to use showbreak (sbr)
vim.opt.breakindentopt = 'sbr'

-- Colorscheme
vim.cmd('colorscheme jellybeans')

-- Highlight settings
-- Set the color of non-text characters
vim.cmd('highlight NonText guifg=#4a4a59')
-- Set the color of special keys (e.g., tabs, spaces, control characters)
vim.cmd('highlight SpecialKey guifg=#4a4a59')

-- Clipboard Settings
if vim.fn.has("mac") == 1 then
  -- Use the system clipboard on macOS
  vim.opt.clipboard = 'unnamed'
else
  -- Use the system clipboard on Linux or other systems
  vim.opt.clipboard = 'unnamedplus'
end

-- Set modeline to scan only 1 line from the top and bottom of the file
vim.opt.modelines = 1

-- Syntax sync for faster syntax highlighting
vim.cmd('syntax sync minlines=256')

-- Disable requirement for JSX extension
vim.g.jsx_ext_required = 0

-- Disable syntax conceal for JSON files
vim.g.vim_json_syntax_conceal = 0

-- Disable Markdown folding
vim.g.vim_markdown_folding_disabled = 1

-- Set filetype to ruby for certain files
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = { "Vagrantfile", "Gemfile", "Capfile" },
  command = "setfiletype ruby"
})

-- Set indentation for specific file types
vim.api.nvim_create_autocmd("FileType", {
  pattern = "ruby",
  command = "setlocal shiftwidth=2 softtabstop=2 expandtab"
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "javascript",
  command = "setlocal shiftwidth=2 softtabstop=2 expandtab"
})

-- Autocommand to set filetype to JavaScript for .es6 files
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = "*.es6",
  command = "setfiletype javascript"
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "yaml",
  command = "setlocal shiftwidth=2 softtabstop=2 expandtab"
})

-- Folding settings
vim.opt.foldmethod = "indent"
vim.opt.foldnestmax = 10
vim.opt.foldenable = false
vim.opt.foldlevel = 1

-- Set grep program to 'ag' if available
if vim.fn.executable('ag') == 1 then
  vim.opt.grepprg = "ag --nogroup --nocolor --column"
  vim.opt.grepformat = "%f:%l:%c%m"
end

-- Ag highlight and ackprg setting
vim.g.ag_highlight = 1
if vim.fn.executable("ag") == 1 then
  vim.g.ackprg = "ag --nogroup --column"
end

-- Textwidth and colorcolumn
vim.opt.textwidth = 120
vim.opt.colorcolumn = { "+1", "+2", "+3", "+4" }

-- Visual bell
vim.opt.visualbell = true

-- Split below
vim.opt.splitbelow = true

-- Cursor shape settings for Tmux
if vim.env.TMUX then
  if vim.fn.has('nvim') == 1 then
    vim.env.NVIM_TUI_ENABLE_CURSOR_SHAPE = 1
  else
    vim.opt.t_SI = "\\<Esc>Ptmux;\\<Esc>\\<Esc>]50;CursorShape=1\\x7\\<Esc>\\"
    vim.opt.t_EI = "\\<Esc>Ptmux;\\<Esc>\\<Esc>]50;CursorShape=0\\x7\\<Esc>\\"
  end
end

-- Auto resize windows on Vim resize
vim.api.nvim_create_autocmd('VimResized', {
  pattern = '*',
  command = 'wincmd ='
})

-- Set formatter path and verbose mode for autoformat
vim.g.formatterpath = { '/usr/local/bin/rubocop', '/Users/oookli/.rvm/gems/ruby-2.3.1/bin/ruby-beautify' }
vim.g.autoformat_verbosemode = 1
