call plug#begin()

Plug 'tpope/vim-sensible'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'rking/ag.vim'
Plug 'mattn/emmet-vim'
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'Valloric/YouCompleteMe'
Plug 'Raimondi/delimitMate'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-vinegar'
Plug 'tmhedberg/matchit'
Plug 'benekastah/neomake'
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
Plug 'ianks/vim-tsx'

call plug#end()

" Map leader to ,
let mapleader = ","

" Macro for deleting blocks
let @f='V$%jd'

" Macro for visualizing blocks
let @v='V$%'

" Macro for navigating blocks
let @n='$%'

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
let g:neomake_coffeelint_maker = {
  \ 'exe': 'coffeelint',
  \ 'args': ['-f', '~/.coffeelint.json'],
  \ }
let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_coffeescript_enabled_makers = ['coffeelint']
autocmd! BufWrite * Neomake

" Indent, syntax, colorscheme and hlsearch
syntax on
set background=dark
let base16colorspace=256
color base16-default
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

" Increase vim yank buffer line limit and size
set viminfo='20,<1000,s1000

" CtrlP settings
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
let g:ctrlp_extensions = ['buffertag']
let g:ctrlp_open_multiple_files = '1r'
let g:ctrlp_by_filename = 1

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

" YCM options
set completeopt-=preview
let g:ycm_add_preview_to_completeopt = 0
let g:ycm_collect_identifiers_from_tags_files = 1

" UltiSnips bindings
let g:UltiSnipsSnippetsDir="~/.config/nvim/UltiSnips"
let g:UltiSnipsExpandTrigger="<c-j>"

" Easytags options
let g:easytags_async = 1
let g:easytags_languages = {
\   'javascript': {
\     'cmd': 'jsctags',
\       'args': [],
\       'fileoutput_opt': '-f',
\       'stdout_opt': '-f-',
\       'recurse_flag': '-R'
\   }
\}

" Hardtime config
autocmd VimEnter,BufNewFile,BufReadPost * silent! call HardTimeOn()
let g:hardtime_ignore_quickfix = 1
let g:hardtime_allow_different_key = 1

" Tab and Shift+Tab cycle through buffers
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>

" Ctrl + G opens CtrlP buffertag search
nnoremap <c-g> :CtrlPBufTag<CR>

" Leader n toggles netrw
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
