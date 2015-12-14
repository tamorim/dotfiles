call plug#begin()

Plug 'tpope/vim-sensible'
Plug 'kien/ctrlp.vim'
Plug 'rking/ag.vim'
Plug 'mattn/emmet-vim'
Plug 'kchmck/vim-coffee-script'
Plug 'chriskempson/base16-vim'
Plug 'bling/vim-airline'
Plug 'Valloric/YouCompleteMe'
Plug 'Raimondi/delimitMate'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-vinegar'
Plug 'wikitopian/hardmode'
Plug 'tmhedberg/matchit'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'hail2u/vim-css3-syntax'
Plug 'groenewege/vim-less'
Plug 'benekastah/neomake'
Plug 'terryma/vim-multiple-cursors'

call plug#end()

" Map leader to ,
let mapleader = ","

" Macro for deleting blocks
let @f='V$%ddd'

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

" Relative line numbers and absolute number on cursor
set relativenumber
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

" Autostart HardMode
autocmd VimEnter,BufNewFile,BufReadPost * silent! call HardMode()

" Tab and Shift+Tab cycle through buffers
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>

" Leader n toggles netrw
nnoremap <Leader>n :e.<CR>

" Leader c deletes current buffer
nnoremap <Leader>c :bdelete<CR>

" Leader a opens ag
nnoremap <Leader>a :Ag 

" Leader h toggles HardMode
nnoremap <Leader>h <Esc>:call ToggleHardMode()<CR>

" Leader e evaluates current file
nnoremap <Leader>e :source %<CR>

" Leader t sets or shows current tab config
nnoremap <Leader>t :Stab<CR>

" Leader v edits nvimrc file
nnoremap <Leader>v :e ~/.config/nvim/init.vim<CR>

" Leader w removes trailing whitespace
nnoremap <Leader>w :%s/\s\+$//g<CR>

