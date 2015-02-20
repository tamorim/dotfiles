et nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" NERDTree plugin
Plugin 'scrooloose/nerdtree'

" Ctrl P Plugin
Plugin 'kien/ctrlp.vim'

" AG Plugin
Plugin 'rking/ag.vim'

" Vim Sensible
Plugin 'tpope/vim-sensible'

" Spacegray colorscheme
Plugin 'Spacegray.vim'

" Emmet Plugin
Plugin 'mattn/emmet-vim'

" CoffeeScript Plugin
Plugin 'kchmck/vim-coffee-script'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

if has('gui_running')
  set guifont=Monaco:h16
endif
colorscheme spacegray
syntax on
filetype on
filetype indent on
filetype plugin on
set number
set ts=4
set softtabstop=4
set cursorline
set foldenable
set foldlevelstart=99
set foldnestmax=10
" space open/closes folds
nnoremap <space> za
set foldmethod=indent
" move vertically by visual line
nnoremap j gj
nnoremap k gk
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup
