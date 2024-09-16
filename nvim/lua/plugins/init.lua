-- Load vim-plug
local Plug = vim.fn['plug#']

vim.call('plug#begin', '~/.vim/plugged')

-- Plugin list
Plug('sheerun/vim-polyglot')
Plug('VundleVim/Vundle.vim')
Plug('tpope/vim-abolish')
Plug('dyng/ctrlsf.vim')
Plug('tpope/vim-rvm')
Plug('tpope/vim-surround')
Plug('scrooloose/nerdtree')
Plug('tpope/vim-commentary')
Plug('tpope/vim-repeat')
Plug('jiangmiao/auto-pairs')
Plug('docunext/closetag.vim')
Plug('chun-yang/vim-action-ag')
Plug('easymotion/vim-easymotion')
Plug('nanotech/jellybeans.vim')
Plug('rking/ag.vim')
Plug('tpope/vim-fugitive')
Plug('bling/vim-airline')
Plug('tpope/vim-projectionist')
Plug('godlygeek/tabular')
Plug('mbbill/undotree')
Plug('ervandew/supertab')
Plug('Matt-Deacalion/vim-systemd-syntax')
Plug('editorconfig/editorconfig-vim')
Plug('moll/vim-node')
Plug('mattn/emmet-vim')
Plug('Valloric/MatchTagAlways')
Plug('plasticboy/vim-markdown')
Plug('tpope/vim-rails')
Plug('tpope/vim-bundler')
Plug('thoughtbot/vim-rspec')
Plug('gabebw/vim-spec-runner')
Plug('tpope/vim-endwise')
Plug('christoomey/vim-tmux-runner')
Plug('christoomey/vim-tmux-navigator')
Plug('neomake/neomake')
Plug('sbdchd/neoformat')
Plug('junegunn/fzf', { dir = '~/.fzf', ['do'] = './install --all' })
Plug('junegunn/fzf.vim')
Plug('rizzatti/dash.vim')
-- Plug('dart-lang/dart-vim-plugin')
Plug('natebosch/vim-lsc')
-- Plug('natebosch/vim-lsc-dart')
Plug('neoclide/coc.nvim', { branch = 'release' })
-- Plug('github/copilot.vim')

--- avante.nvim
-- Deps
Plug('MeanderingProgrammer/render-markdown.nvim')
Plug('stevearc/dressing.nvim')
Plug('nvim-lua/plenary.nvim')
Plug('MunifTanjim/nui.nvim')

-- Optional Dependencies
Plug('nvim-tree/nvim-web-devicons') -- or Plug 'echasnovski/mini.icons'
Plug('HakonHarnes/img-clip.nvim')
-- Plug('zbirenbaum/copilot.lua')

-- Plugin with build instructions and lazy loading
-- Plug('yetone/avante.nvim', { branch = 'main', source = 'true', ['do'] = function() vim.fn['avante#build']() end })
Plug('yetone/avante.nvim', { branch = 'main', ['do'] = function() vim.fn['avante#build']('source=true') end })
--- /avante.nvim

vim.call('plug#end')

-- Load plugin configurations
require('plugins.ctrlsf')    -- Load CtrlSF settings
require('plugins.fzf')       -- Load FZF settings
require('plugins.nerdtree')  -- Load NERDTree settings
require('plugins.easymotion')  -- Load NERDTree settings
require('plugins.neomake')  -- Load Neomake settings
require('plugins.neoformat')  -- Load Neoformat settings
require('plugins.tmux-runner')  -- Load TMUX runner settings
require('plugins.spec-runner')  -- Load TMUX runner settings
require('plugins.undotree')  -- Load UndoTree settings
require('plugins.lsc-and-coc') -- Load vim-lsc and coc.nvim settings
require('plugins.render-markdown')
