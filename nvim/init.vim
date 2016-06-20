" Setting up Plug
let isPlugPresent = 1
let plug_vim = expand('~/.config/nvim/autoload/plug.vim')

if !filereadable(plug_vim)
    echo "Installing Plug..."
    silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    let isPlugPresent = 0
endif

if isPlugPresent == 0
    echo "Installing plugins..."
    echo ""
    silent! source ~/.config/nvim/init.vim
    silent! PlugInstall
    silent! bdelete
endif

function! DoRemote(arg)
  UpdateRemotePlugins
endfunction

call plug#begin()

Plug 'ctrlpvim/ctrlp.vim'
Plug 'mattn/emmet-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'Raimondi/delimitMate'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tmhedberg/matchit'
Plug 'neomake/neomake'
Plug 'terryma/vim-multiple-cursors'
Plug 'jimmyhchan/dustjs.vim'
Plug 'sjl/gundo.vim'
Plug 'tpope/vim-fugitive'
Plug 'SirVer/ultisnips'
Plug 'moll/vim-node'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-easytags'
Plug 'takac/vim-hardtime'
Plug 'sheerun/vim-polyglot'
Plug 'othree/yajs.vim'
Plug 'othree/es.next.syntax.vim'
Plug 'ianks/vim-tsx'
Plug 'w0ng/vim-hybrid'
Plug 'vim-scripts/paredit.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'scrooloose/nerdtree'
Plug 'matze/vim-move'
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }
Plug 'carlitux/deoplete-ternjs'
Plug 'rschmukler/pangloss-vim-indent'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

call plug#end()

" Map leader to ,
let mapleader = ","

" Macro for deleting blocks
let @f='V$%jd'

" Macro for visualizing blocks
let @v='V$%'

" Macro for navigating blocks
let @n='$%'

" Macro for deleting stuff surrounding blocks
let @s='0$wviB<b%dddd'

" Set tabstop, softtabstop and shiftwidth to the same value
command! -nargs=* Stab call Stab()
function! Stab()
  let l:tabstop = 1 * input('set tabstop = softtabstop = shiftwidth = ')
  if l:tabstop > 0
    let &l:sts = l:tabstop
    let &l:ts = l:tabstop
    let &l:sw = l:tabstop
  endif
  call SummarizeTabs()
endfunction

function! SummarizeTabs()
  try
    echohl ModeMsg
    echon ' tabstop='.&l:ts
    echon ' shiftwidth='.&l:sw
    echon ' softtabstop='.&l:sts
    if &l:et
      echon ' expandtab'
    else
      echon ' noexpandtab'
    endif
  finally
    echohl None
  endtry
endfunction

" Neomake config
let g:neomake_open_list = 2
let g:neomake_list_height = 5
let eslint_exe = './node_modules/.bin/eslint'
let g:neomake_javascript_eslint_exe = eslint_exe
let g:neomake_jsx_eslint_exe = eslint_exe
let g:neomake_typescript_mytsc_maker = {
\ 'exe': './node_modules/.bin/tsc',
\ 'args': ['@.tscconfig'],
\ 'errorformat': '%E%f %#(%l\,%c): error %m,' .
\ '%E%f %#(%l\,%c): %m,' .
\ '%Eerror %m,' .
\ '%C%\s%\+%m'
\ }
let g:neomake_tsx_mytsc_maker = {
\ 'exe': './node_modules/.bin/tsc',
\ 'args': ['@.tscconfig'],
\ 'errorformat': '%E%f %#(%l\,%c): error %m,' .
\ '%E%f %#(%l\,%c): %m,' .
\ '%Eerror %m,' .
\ '%C%\s%\+%m'
\ }

if filereadable(eslint_exe)
  let g:neomake_javascript_enabled_makers = ['eslint']
  let g:neomake_jsx_enabled_makers = ['eslint']
else
  let g:neomake_javascript_enabled_makers = []
  let g:neomake_jsx_enabled_makers = []
endif

let g:neomake_coffeescript_enabled_makers = ['coffeelint']
let g:neomake_typescript_enabled_makers = ['mytsc']
let g:neomake_tsx_enabled_makers = ['mytsc']
autocmd! BufWinEnter,BufWrite * Neomake
autocmd! QuitPre * let g:neomake_verbose = 0

" Indent, syntax, colorscheme and hlsearch
syntax on
set background=dark
let g:hybrid_custom_term_colors = 1
let g:hybrid_reduced_contrast = 1
colorscheme hybrid
filetype on
filetype indent on
set nohlsearch

" Line numbers
set number

" Cursor line
set cursorline

" Fold setup
set foldenable
set foldlevelstart=99
set foldnestmax=10

" Space open/closes folds
nnoremap <space> za
set foldmethod=indent

" Move vertically by visual line
nnoremap j gj
nnoremap k gk

" Backup
set backup
set backupcopy=yes
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup

" Persistent undo config
set undofile
set undodir=$HOME/.nvim/undo
set undolevels=1000
set undoreload=10000

" Increase vim yank buffer line limit and size
set viminfo='20,<1000,s1000

" CtrlP settings
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_user_command = [
\ '.git',
\ 'bash -c "cd %s && git ls-files . -co --exclude-standard"',
\ 'ag %s -l --nocolor --hidden -g ""'
\ ]
let g:ctrlp_extensions = ['buffertag']
let g:ctrlp_open_multiple_files = '1r'

" Function to auto refresh CtrlP
function! SetupCtrlP()
  if exists("g:loaded_ctrlp") && g:loaded_ctrlp
    augroup CtrlPExtension
      autocmd!
      autocmd FocusGained  * CtrlPClearCache
      autocmd BufWritePost * CtrlPClearCache
    augroup END
  endif
endfunction

if has("autocmd")
  autocmd VimEnter * :call SetupCtrlP()
endif

" When switching buffers, only hide the old one
" Makes undo persist on buffer switch
set hidden

" Enables vim airline tabline
let g:airline#extensions#tabline#enabled = 1

" Enables Powerline symbols
let g:airline_powerline_fonts = 1

" Enable vim-jsx on js files
let g:jsx_ext_required = 0

" Make the 81st column stand out
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)

" Show tab characters, trailing whitespace etc
set listchars=tab:>~,nbsp:_,trail:~
set list

" UltiSnips bindings
let g:UltiSnipsSnippetsDir="~/.config/nvim/UltiSnips"
let g:UltiSnipsExpandTrigger="<c-j>"

" Easytags options
let g:easytags_async = 1
let g:easytags_languages = {
\  'javascript': {
\    'cmd': 'jsctags',
\      'args': [],
\      'fileoutput_opt': '-f',
\      'stdout_opt': '-f-',
\      'recurse_flag': '-R'
\  }
\}

" Hardtime config
autocmd VimEnter,BufNewFile,BufReadPost * silent! call HardTimeOn()
let g:hardtime_ignore_quickfix = 1
let g:hardtime_allow_different_key = 1

" EditorConfig config
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

" deoplete config
let g:deoplete#enable_at_startup = 1

" deoplete tab-complete
inoremap <silent><expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <silent><expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Disable vim-polyglot on javascript files
let g:polyglot_disabled = ['javascript']

" Skip location list and quick fix list on buffer switch
augroup qf
  autocmd!
  autocmd FileType qf set nobuflisted
augroup END

" Tab and Shift+Tab cycle through buffers
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>

" Ctrl + G opens CtrlP buffertag search
nnoremap <c-g> :CtrlPBufTag<CR>

" Leader n toggles netrw or NERDTree if installed
nnoremap <Leader>n :e.<CR>

" Leader c deletes current buffer
nnoremap <Leader>c :bdelete<CR>

" Leader a opens ag
nnoremap <Leader>a :Ag 

" Leader u toggles Gundo
nnoremap <Leader>u :GundoToggle<CR>

" Leader e evaluates current file
nnoremap <Leader>e :source %<CR>

" Leader t sets or shows current tab config
nnoremap <Leader>t :Stab<CR>

" Leader v edits nvimrc file
nnoremap <Leader>v :e ~/.config/nvim/init.vim<CR>

" Leader w removes trailing whitespace
nnoremap <Leader>w :%s/\s\+$//g<CR>

" Leader i indents a JSX component
vnoremap <Leader>i :s/\zs\s\ze\w\+-\?/\="\n".matchstr(getline('.'), '^\s*').'  '/g \| s/\v\s?(\/?\>)/\="\n".matchstr(getline('.'), '^\s*').submatch(1)/ \| normal <<<CR>
