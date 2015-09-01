call plug#begin()

Plug 'tpope/vim-sensible'
Plug 'kien/ctrlp.vim'
Plug 'rking/ag.vim'
Plug 'mattn/emmet-vim'
Plug 'kchmck/vim-coffee-script'
Plug 'chankaward/vim-railscasts-theme'
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

call plug#end()

" Map leader to ,
let mapleader = ","

" Font, indent, syntax and coloscheme
if has('gui_running')
	if has("gui_macvim")
		set guifont=Monaco:h16
		set fu
	elseif has("gui_gtk2")
		set guifont=Monaco\ 10
		set guioptions-=m  "remove menu bar
		set guioptions-=T  "remove toolbar
	endif

	set guioptions-=r  "remove right-hand scroll bar
	set guioptions-=L  "remove left-hand scroll bar
endif
syntax on
color railscasts
filetype on
filetype indent on

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

" Fix git-sh-setup in MacVim
if has("gui_macvim")
    set shell=/bin/bash\ -l
endif

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

" Tab and Shift+Tab cycle through buffers
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>

" Ctrl+N toggles NERDTree
nnoremap <C-n> :NERDTreeToggle<CR>

" Leader c deletes current buffer
nnoremap <Leader>c :bdelete<CR>

" Leader a opens ag
nnoremap <Leader>a :Ag

