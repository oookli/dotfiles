filetype off

let mapleader="\<Space>"

" # Plugins
call plug#begin('~/.vim/plugged')

Plug 'sheerun/vim-polyglot'
Plug 'VundleVim/Vundle.vim'
Plug 'tpope/vim-abolish'
Plug 'dyng/ctrlsf.vim'
" Plug 'tommcdo/vim-exchange'
" Plug 'terryma/vim-multiple-cursors'
" Plug 'Shougo/vimproc.vim'
Plug 'tpope/vim-rvm'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
" Plug 'suy/vim-context-commentstring'
Plug 'tpope/vim-repeat'
Plug 'jiangmiao/auto-pairs'
Plug 'docunext/closetag.vim'
Plug 'chun-yang/vim-action-ag'
Plug 'easymotion/vim-easymotion'
Plug 'nanotech/jellybeans.vim'
Plug 'rking/ag.vim'
Plug 'tpope/vim-fugitive'
Plug 'bling/vim-airline'
" Plug 'leafgarland/typescript-vim'
" Plug 'jeetsukumaran/vim-buffergator'
Plug 'tpope/vim-projectionist'
" Plug 'tpope/vim-dispatch'
" Plug 'majutsushi/tagbar'
Plug 'godlygeek/tabular'
Plug 'mbbill/undotree'
Plug 'ervandew/supertab'
Plug 'Matt-Deacalion/vim-systemd-syntax'
Plug 'editorconfig/editorconfig-vim'
" Plug 'stefanoverna/vim-i18n'
" Plug 'elzr/vim-json'
" Plug 'pangloss/vim-javascript'
" Plug 'mxw/vim-jsx'
" Plug 'peitalin/vim-jsx-typescript'
Plug 'moll/vim-node'
Plug 'mattn/emmet-vim'
Plug 'Valloric/MatchTagAlways'
Plug 'plasticboy/vim-markdown'
" Plug 'tpope/vim-haml'
" Plug 'nono/vim-handlebars'
" Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-bundler'
Plug 'thoughtbot/vim-rspec'
Plug 'gabebw/vim-spec-runner'
Plug 'tpope/vim-endwise'
Plug 'christoomey/vim-tmux-runner'
Plug 'christoomey/vim-tmux-navigator'
" Plug 'scrooloose/syntastic'
Plug 'neomake/neomake'
" Plug 'chrisbra/csv.vim'
" Plug 'keith/swift.vim'
Plug 'othree/yajs.vim', { 'for': 'javascript' }
" Plug 'Chiel92/vim-autoformat'
Plug 'sbdchd/neoformat'
" Plug 'w0rp/ale'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'rizzatti/dash.vim'
" Plug 'ludovicchabant/vim-gutentags'

Plug 'dart-lang/dart-vim-plugin'
Plug 'natebosch/vim-lsc'
Plug 'natebosch/vim-lsc-dart'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'github/copilot.vim'

call plug#end()

filetype plugin indent on

" General configurations
"------------------------

set hidden                      " Allow buffer change w/o saving
set autoread                    " Load file from disk, ie for git reset
set lazyredraw                  " Don't update while executing macros
set backspace=indent,eol,start	" Sane backspace behavior
set history=1000  		          " Remember last 1000 commands
set scrolloff=4                 " Keep at least 4 lines below cursor
set expandtab                   " Convert <tab> to spaces (2 or 4)
set tabstop=2                   " Two spaces per tab as default
set shiftwidth=2                "     then override with per filteype
set softtabstop=2               "     specific settings via autocmd
set secure                      " Limit what modelines and autocmds can do
set autowrite                   " write for me when I take any action

set relativenumber
if v:version > 703
  set number " hybrid relative and absolute for current line
endif

function! Numbertoggle()
  if exists('+relativenumber')
    setl rnu!
  else
    setl nu!
  endif
endfunction
nnoremap <f12> :call Numbertoggle()<cr>

" Disable sound/visual bell on errors
" May need additional config about audible bell
" set t_vb=

" Set modeline to 1 to allow rcfiles to be recognized as vim files
set modelines=1

" I save constantly and hate swap files in my prject dirs
set nobackup
set nowritebackup
set noswapfile

" Persistent undo
let undodir = expand('~/.undo-vim')
if !isdirectory(undodir)
  call mkdir(undodir)
endif
set undodir=~/.undo-vim
set undofile " Create FILE.un~ files for persistent undo
"------------------------
" End General configurations

" Mappings for quick search & replace. Global set to default
" Do a / search first, then leave pattern empty in :s// to use previous
nnoremap <Leader>sub :%s///g<left><left>
vnoremap <Leader>sub :s///g<left><left>
nnoremap <leader>wub :%s//<C-r><C-w>/g<cr>

nmap <leader>vr :sp $MYVIMRC<cr>
nmap <leader>so :source $MYVIMRC<cr>

nnoremap <silent> <leader><leader>  <esc>:w<cr>

" tmux runner
let g:vtrusevtrmaps = 1
let g:vtrgitcduponopen = 0
let g:vtrpercentage = 50

" nmap <leader>fs :vtrflushcommand<cr>:vtrsendcommandtorunner<cr>
" nmap <c-f> :vtrsendlinestorunner<cr>
" vmap <c-f> :vtrsendlinestorunner<cr>

" nmap <leader>osr :vtropenrunner { 'orientation': 'h', 'percentage': 50 }<cr>
" nmap <leader>opr :vtropenrunner { 'orientation': 'h', 'percentage': 50, 'cmd': 'pry'  }<cr>

" nnoremap <leader>sd :vtrsendctrld<cr>
" nnoremap <leader>sq :vtrsendcommandtorunner q<cr>
" nnoremap <leader>sl :vtrsendcommandtorunner <cr>
" nnoremap <leader>scu :vtrsendcommandtorunner <cr>

" let g:vtr_filetype_runner_overrides = {
"   \ 'haskell': 'ghci {file}',
"   \ 'applescript': 'osascript {file}'
"   \ }

let g:spec_runner_dispatcher = "vtrsendcommand! be {command}"

" map <leader>rt <plug>runfocusedspec
" map <leader>lt <plug>runmostrecentspec# clipboard

" macos vs linux clipboard
if has("mac")
  set clipboard=unnamed
else
  set clipboard=unnamedplus
endif


" backup/persistance settings
" set backupdir=$home/.vim/tmp/backup//
" set directory=$home/.vim/tmp/swap//
" set backupskip=/tmp/*,/private/tmp/*"
" set backup
" set writebackup

set hlsearch
set ignorecase
set smartcase
set wrap
set linebreak
let &showbreak = '↳ '
set breakindent
set breakindentopt=sbr
set list
" shortcut to rapidly toggle `set list`
nmap <leader>l :set list!<cr>

" use the same symbols as textmate for tabstops and eols
set listchars=tab:>·,eol:¬,trail:·
highlight nontext guifg=#4a4a59
highlight specialkey guifg=#4a4a59
set t_co=256
colorscheme jellybeans

syntax sync minlines=256

let g:jsx_ext_required = 0

" let g:slime_target = "tmux"

let g:vim_json_syntax_conceal = 0

" let g:acp_enableatstartup = 0
" let g:neocomplete#enable_at_startup = 1
" let g:neocomplete#enable_smart_case = 1

" let g:neocomplete#sources#syntax#min_keyword_length = 3
" let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" if !exists('g:neocomplete#keyword_patterns')
"   let g:neocomplete#keyword_patterns = {}
" endif
" let g:neocomplete#keyword_patterns['default'] = '\h\w*'


" enable omni completion.
" autocmd filetype css setlocal omnifunc=csscomplete#completecss
" autocmd filetype html,markdown setlocal omnifunc=htmlcomplete#completetags
" autocmd filetype javascript setlocal omnifunc=javascriptcomplete#completejs
" autocmd filetype python setlocal omnifunc=pythoncomplete#complete
" autocmd filetype xml setlocal omnifunc=xmlcomplete#completetags
" autocmd filetype ruby setlocal omnifunc=rubycomplete#completetags

" let g:buffergator_viewport_split_policy = "b"
" let g:buffergator_sort_regime = "mru"

let g:vim_markdown_folding_disabled = 1

function! NeomakeESlintChecker()
  for folder in ['', 'frontend', 'frontend_auto', 'frontend_home']
    let l:eslint = $PWD . '/' . folder . '/node_modules/.bin/eslint'

    if executable(l:eslint)
      break
    endif
  endfor

  let b:neomake_javascript_eslint_exe = l:eslint
endfunction

autocmd FileType javascript :call NeomakeESlintChecker()

let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_jsx_enabled_makers = ['eslint']
let g:neomake_haml_enabled_makers = ['hamllint']
let g:neomake_ruby_enabled_makers = ['rubocop', 'mri']
let g:neomake_ruby_rubocop_exe = 'bundle'
let g:neomake_ruby_rubocop_args = ['exec', 'rubocop']
" let g:syntastic_erlang_checkers = ['syntaxerl']

autocmd! BufWritePost * Neomake

" imap <C-k>     <Plug>(neosnippet_expand_or_jump)
" smap <C-k>     <Plug>(neosnippet_expand_or_jump)
" xmap <C-k>     <Plug>(neosnippet_expand_target)

let g:tagbar_autofocus = 1
nnoremap <silent><leader>] :TagbarToggle<CR>
nnoremap <F5> :UndotreeToggle<cr>
" export TERMINFO="$HOME/.terminfo"
" Map ctrl-movement keys to window switching
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-l> <C-w>l
nnoremap <C-h> <C-w>h

" Toggle paste mode
nmap <silent> <F4> :set invpaste<CR>:set paste?<CR>
imap <silent> <F4> <ESC>:set invpaste<CR>:set paste?<CR>

" Use j/k to start, then scroll through autocomplete options
inoremap <expr> <C-j> ((pumvisible())?("\<C-n>"):("\<C-x><c-n>"))
inoremap <expr> <C-k> ((pumvisible())?("\<C-p>"):("\<C-x><c-k>"))

" Do not use <Ctrl-c> to break out to normal mode
" Use C-Space to Esc out of any mode
nnoremap <C-Space> <Esc>:noh<CR>
vnoremap <C-Space> <Esc>gV
onoremap <C-Space> <Esc>
cnoremap <C-Space> <C-c>
inoremap <C-Space> <Esc>
" Terminal sees <C-@> as <C-space> WTF, but ok
nnoremap <C-@> <Esc>:noh<CR>
vnoremap <C-@> <Esc>gV
onoremap <C-@> <Esc>
cnoremap <C-@> <C-c>
inoremap <C-@> <Esc>

" Easy re-sourcing of vimrc
map <leader>sop :source %<cr>

" Doesn't seem I'm going to win on this one
command! W write

" Easy navigation for editing and cding
nnoremap <leader>R :e <C-r>=<cr>

function! s:getCurrentDirectory()
  let relative_dir = substitute(expand("%:p:h"), getcwd()."/", "", "")."/"
  call s:CurrentDirectory(relative_dir)
endfunction

" Delete current file and current buffer
" map <leader>fd :call "delete(expand('%')) | bdelete!"

" """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" " RENAME CURRENT FILE (thanks Gary Bernhardt)
" """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" function! RenameFile()
"   let old_name = expand('%')
"   let new_name = input('New file name: ', expand('%'), 'file')
"   if new_name != '' && new_name != old_name
"     exec ':saveas ' . new_name
"     exec ':silent !rm ' . old_name
"     redraw!
"   endif
" endfunction
" map <Leader>n :call RenameFile()<cr>

"Make Y yank to end of line (like D, or C)
nmap Y y$

" Easy access to the start of the line
nnoremap 0 ^

" Leader-; to enter command mode. No more holding shift!
" nnoremap <leader>; :
" vnoremap <leader>; :

" Quickly open a second window to view files side by side
nmap <LEADER>vn :vsplit<CR>
nmap <LEADER>vs :vsplit<CR>

" Move one line at a time, aka 'fine ajdustment'
" nnoremap j gj
" nnoremap k gk

" Reselect pasted text. Mnem: 'Get pasted'
" nnoremap gp `[v`]

" Go to position of last edit. Mnem: 'Go to Edit'
nnoremap ge `.

" Tabs configurations
"------------------------

for tab_number in [1,2,3,4, 5, 6]
  execute 'map <silent> <C-w>' . tab_number . ' :tabnext ' . tab_number . '<cr>'
endfor

nnoremap <leader>wn :tabnew<cr>
nnoremap <leader>ct :tabclose<cr>

" Tags configurations
"--------------------

au FileType * call EnableTagNavMaps()

function! EnableTagNavMaps()
  if s:BufferIsTagNavigable()
    nmap <buffer> <cr> <c-]>
    nmap <buffer> <bs> <c-t>
  endif
endfunction

let s:navigable_filetypes = ['vim', 'ruby', 'javascript', 'sh']

function! s:BufferIsTagNavigable()
  let is_navigable_filetype = index(s:navigable_filetypes, &filetype) != -1
  let is_normal_buffer = &buftype !=? 'nofile'
  return is_navigable_filetype && is_normal_buffer
endfunction

set tags+=./git/tags

" format the entire file
nnoremap <leader>fef :normal! gg=G``<CR>

" set text wrapping toggles
" nmap <silent> <leader>tw :set invwrap<CR>:set wrap?<CR>

" find merge conflict markers
" nmap <silent> <leader>fc <ESC>/\v^[<=>]{7}( .*\|$)<CR>

" upper/lower word
" nmap <leader>u mQviwU`Q
" nmap <leader>l mQviwu`Q

map <Leader>nr :NERDTreeFind<CR>
nmap <C-a> :NERDTreeToggle<CR>
let NERDTreeQuitOnOpen=1
let NERDTreeIgnore = ['\.pyc$']
" prevent copy filename from NerdTree with ^G symbol
let NERDTreeNodeDelimiter="\u00a0"

nmap <silent> // :nohlsearch<CR>
noremap ,hl :set hlsearch! hlsearch?<CR>

" Allows you to enter sudo pass and save the file
" " when you forgot to open your file with sudo
cmap w!! %!sudo tee > /dev/null %

" Allow to copy/paste between VIM instances
" "copy the current visual selection to ~/.vbuf
" vmap <Leader>y :w! ~/.vbuf<CR>
" "copy the current line to the buffer file if no visual selection
" nmap <Leader>y :.w! ~/.vbuf<CR>
" "paste the contents of the buffer file
" nmap <Leader>p :r ~/.vbuf<CR>
nnoremap <Leader>, :CtrlSF<Space>
nnoremap <leader>; :CtrlSF -R -I<Space>
nmap <silent> <Leader>d <Plug>DashGlobalSearch
let g:ctrlsf_extra_backend_args = {
\ 'ag': '--ignore node_modules --ignore dist'
\ }

" Automatically removing all trailing whitespace
autocmd BufWritePre * :%s/\s\+$//e
au BufRead,BufNewFile {Vagrantfile,Gemfile,Capfile} set ft=ruby

au FileType ruby setl sw=2 sts=2 et
au FileType javascript setl sw=2 sts=2 et
au FileType yaml setl sw=2 sts=2 et
if executable('ag')
    " Note we extract the column as well as the file and line number
    set grepprg=ag\ --nogroup\ --nocolor\ --column
    set grepformat=%f:%l:%c%m
endif
nmap <silent> <RIGHT> :cnext<CR>
nmap <silent> <LEFT> :cprev<CR>
" map <C-UP> :bn<cr>
" map <C-DOWN> :bp<cr>
map <Leader>x :bd<cr>
set foldmethod=indent
set foldnestmax=10
set nofoldenable
set foldlevel=1


" save session to home directory
map <F2> :mksession! ~/vim_session <cr> " Quick write session with F2
map <F3> :source ~/vim_session <cr>     " And load session with F3

let g:ag_highlight=1
if executable("ag")
  let g:ackprg = "ag --nogroup --column"
endif

" insert new line in normal mode
nnoremap <Leader>J :set paste<CR>m`o<Esc>``:set nopaste<CR>
nnoremap <Leader>K :set paste<CR>m`O<Esc>``:set nopaste<CR>
nnoremap <Leader>L i<space><esc>

set guifont=Monaco:h12

" easymotion settings
let g:EasyMotion_do_mapping = 0
" Jump to anywhere you want with minimal keystrokes, with just one key binding.
" `s{char}{label}`
nmap <Tab><Tab> <Plug>(easymotion-overwin-f)
" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1

" Emmet
let g:user_emmet_leader_key="<c-e>"
" let g:user_emmet_install_global = 0
" autocmd FileType html,haml,css,scss EmmetInstall
" autocmd FileType html imap <tab> <plug>(emmet-expand-abbr)
" autocmd FileType haml imap <tab> <plug>(emmet-expand-abbr)
" autocmd FileType css imap <tab> <plug>(emmet-expand-abbr)
" autocmd FileType scss imap <tab> <plug>(emmet-expand-abbr)

" multicursor settings
let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_next_key='<C-n>'
let g:multi_cursor_prev_key='<C-j>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'

if has('gui_running')
    set guioptions-=r "Hide the right side scrollbar
    set guioptions-=L "Hide the left side scrollbar
    set guioptions-=T "Hide toolbars...this is vim for craps sake
    set guioptions-=m "Hide the menu, see above

    " Size and position the window well (only perform on startup)
    if !exists('g:vimrc_loaded')
        set columns=85
        set lines=999
        winpos 999 5
    endif

    " Hightlight the current row. Index-guide plugin covers columns
    set cursorline

    " MacVim is very pretty
    if has('gui_macvim')
        " set transparency=8

        " Fullscreen options
        set fuoptions=maxvert
        " au GUIEnter * set fullscreen
    endif

    " Set a pretty font
    if has('win32')
        set guifont=Consolas:h10
    elseif has('mac')
        if !exists('g:vimrc_loaded')
          set guifont=Menlo:h12
        endif
    endif
else
    set nocursorline nocursorcolumn
endif

set textwidth=120
set colorcolumn=+1,+2,+3,+4

set vb

" Easy access to maximizing
nnoremap <C-_> <C-w>_

set splitbelow

" change cursor in insert mode
if exists('$TMUX')
  if has('nvim')
    let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
  else
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
  endif
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" automatically rebalance windows on vim resize
autocmd VimResized * :wincmd =

" zoom a vim pane, <C-w>= to re-balance
nnoremap <leader>- :wincmd _<cr>:wincmd \|<cr>
nnoremap <leader>= :wincmd =<cr>

" List out the syntax for the word under the cursor
function! s:SyntaxGroup()
    let hl = synIDattr(synID(line("."),col("."),1),"name")
    let trans = synIDattr(synID(line("."),col("."),0),"name")
    let low = synIDattr(synIDtrans(synID(line("."),col("."),1)),"name")
    " for id in synstack(line("."), col("."))
       " echo synIDattr(id, 'name')
    " endfor
    echo 'hl<' . hl . '> trans<' . trans . '>low<' . low . '>'
endfunction
command! SyntaxGroupUnderCursor call <sid>SyntaxGroup()
nnoremap <LEADER>sg :SyntaxGroupUnderCursor<CR>

nnoremap <leader>! :redraw!<cr>
nnoremap <leader>u :e!<cr>

nnoremap <leader>mwh <C-w>H
nnoremap <leader>mwj <C-w>J
nnoremap <leader>mwk <C-w>K
nnoremap <leader>mwl <C-w>L

let g:spec_runner_dispatcher = "VtrSendCommand! {command}"
let g:VtrOrientation = "h"
map <leader>t <Plug>RunFocusedSpec

vmap <Leader>et :call I18nTranslateString()<CR>
nnoremap <silent> <BS> :TmuxNavigateLeft<cr>

au BufRead,BufNewFile *.es6 setfiletype javascript

set shortmess=I

let g:formatterpath = [ '/usr/local/bin/rubocop', '/Users/oookli/.rvm/gems/ruby-2.3.1/bin/ruby-beautify' ]
let g:autoformat_verbosemode=1

vnoremap ;rv c<C-O>:set revins<CR><C-R>"<Esc>:set norevins<CR>

let g:fzf_files_options =
  \ '--reverse ' .
  \ '--preview "(coderay {} || cat {}) 2> /dev/null | head -'.&lines.'"'

let g:fzf_buffers_jump = 1
let g:fzf_history_dir = '~/.local/share/fzf-history'
let $FZF_DEFAULT_COMMAND = 'ag -g "" --hidden'
nnoremap <silent> <expr> <C-p> (expand('%') =~ 'NERD_tree' ? "\<C-a>\<C-a>" : '').":Files\<CR>"
nnoremap <silent> <expr> <leader>p (expand('%') =~ 'NERD_tree' ? "\<C-a>\<C-a>" : '').":History\<CR>"
nnoremap <silent> <expr> <leader>\ (expand('%') =~ 'NERD_tree' ? "\<C-a>\<C-a>" : '').":Buffers\<CR>"

let branch_files_options = { 'source': 'branch_files' }
let uncommited_files_options = { 'source': 'branch_files -w' }

let s:diff_options =
  \ '--reverse ' .
  \ '--preview "(mdiff {} | tail -n +5 || cat {}) 2> /dev/null | head -'.&lines.'"'
command! BranchFiles call fzf#run(fzf#wrap('BranchFiles',
      \ extend(branch_files_options, { 'options': s:diff_options }), 0))
command! UncommitedFiles call fzf#run(fzf#wrap('UncommitedFiles',
      \ extend(uncommited_files_options, { 'options': s:diff_options }), 0))
nnoremap <silent> <leader>gp :BranchFiles<cr>
nnoremap <silent> <leader>GP :UncommitedFiles<cr>

let g:gutentags_add_default_project_roots = 0
let g:gutentags_project_root = ['package.json', '.git']

let g:gutentags_cache_dir = expand('~/.cache/vim/ctags/')

command! -nargs=0 GutentagsClearCache call system('rm ' . g:gutentags_cache_dir . '/*')

let g:gutentags_generate_on_new = 1
let g:gutentags_generate_on_missing = 1
let g:gutentags_generate_on_write = 1
let g:gutentags_generate_on_empty_buffer = 0

let g:gutentags_ctags_extra_args = [
      \ '--tag-relative=yes',
      \ '--fields=+ailmnS',
      \ ]

let g:gutentags_ctags_exclude = [
      \ '*.git', '*.svg', '*.hg',
      \ '*/tests/*',
      \ 'packs-test',
      \ 'build',
      \ 'dist',
      \ '*sites/*/files/*',
      \ 'bin',
      \ 'node_modules',
      \ 'bower_components',
      \ 'cache',
      \ 'compiled',
      \ 'docs',
      \ 'example',
      \ 'bundle',
      \ 'vendor',
      \ '*.md',
      \ '*-lock.json',
      \ '*.lock',
      \ '*bundle*.js',
      \ '*build*.js',
      \ '.*rc*',
      \ '*.json',
      \ '*.min.*',
      \ '*.map',
      \ '*.bak',
      \ '*.zip',
      \ '*.pyc',
      \ '*.class',
      \ '*.sln',
      \ '*.Master',
      \ '*.csproj',
      \ '*.tmp',
      \ '*.csproj.user',
      \ '*.cache',
      \ '*.pdb',
      \ 'tags*',
      \ 'cscope.*',
      \ '*.css',
      \ '*.less',
      \ '*.scss',
      \ '*.exe', '*.dll',
      \ '*.mp3', '*.ogg', '*.flac',
      \ '*.swp', '*.swo',
      \ '*.bmp', '*.gif', '*.ico', '*.jpg', '*.png',
      \ '*.rar', '*.zip', '*.tar', '*.tar.gz', '*.tar.xz', '*.tar.bz2',
      \ '*.pdf', '*.doc', '*.docx', '*.ppt', '*.pptx',
      \ ]

function! s:tags_sink(line)
  let parts = split(a:line, '\t\zs')
  let excmd = matchstr(parts[2:], '^.*\ze;"\t')
  execute 'silent e' parts[1][:-2]
  let [magic, &magic] = [&magic, 0]
  execute excmd
  let &magic = magic
endfunction

autocmd BufWritePre *.js Neoformat
autocmd BufWritePre *.ts Neoformat
autocmd BufWritePre *.tsx Neoformat
autocmd BufWritePre *.scss Neoformat
" let g:neoformat_try_formatprg = 1
let g:neoformat_run_all_formatters = 1
let g:neoformat_only_msg_on_error = 1
let g:neoformat_try_node_exe = 1
" let g:neoformat_verbose = 1

" autocmd! FileType fzf tnoremap <buffer> <esc> <C-Space>


" autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescript.jsx

" let g:context#commentstring#table['javascript.jsx'] = {
"       \ 'jsComment' : '// %s',
"       \ 'jsImport' : '// %s',
"       \ 'jsxStatment' : '// %s',
"       \ 'jsxRegion' : '{/*%s*/}',
" \}

let g:lsc_auto_map = v:true

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

inoremap <expr> <Tab> pumvisible() ? coc#_select_confirm() : "<Tab>"
