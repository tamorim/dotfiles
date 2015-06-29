set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Vim Sensible
Plugin 'tpope/vim-sensible'

" NERDTree
Plugin 'scrooloose/nerdtree'

" Ctrl P
Plugin 'kien/ctrlp.vim'

" AG
Plugin 'rking/ag.vim'

" Emmet
Plugin 'mattn/emmet-vim'

" CoffeeScript
Plugin 'kchmck/vim-coffee-script'

" Railscasts colorscheme
Plugin 'chankaward/vim-railscasts-theme'

" Airline
Plugin 'bling/vim-airline'

" YouCompleteMe
Plugin 'Valloric/YouCompleteMe'

" delimitMate
Plugin 'Raimondi/delimitMate'

" vim-stylus
Plugin 'wavded/vim-stylus'

" Git-gutter
Plugin 'airblade/vim-gitgutter'

" vim-surround
Plugin 'tpope/vim-surround'

" Vim Sleuth
Plugin 'tpope/vim-sleuth'

" Vim Commentary
Plugin 'tpope/vim-commentary'

" All of your Plugins must be added before the following line
call vundle#end()            " required
" To plugin indent changes
filetype plugin on    " required

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

" Ctrl+X closes current buffer
nnoremap <C-x> :bdelete<CR>

" Ctrl+N toggles NERDTree
nnoremap <C-n> :NERDTreeToggle<CR>

