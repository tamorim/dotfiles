set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Vim Sensible plugin
Plugin 'tpope/vim-sensible'

" NERDTree plugin
Plugin 'scrooloose/nerdtree'

" Ctrl P Plugin
Plugin 'kien/ctrlp.vim'

" AG Plugin
Plugin 'rking/ag.vim'

" Emmet Plugin
Plugin 'mattn/emmet-vim'

" CoffeeScript Plugin
Plugin 'kchmck/vim-coffee-script'

" Dracula colorscheme
Plugin 'zenorocha/dracula-theme', {'rtp': 'vim/'}

" Airline Plugin
Plugin 'bling/vim-airline'

" YouCompleteMe Plugin
Plugin 'Valloric/YouCompleteMe'

" delimitMate Plugin
Plugin 'Raimondi/delimitMate'

" vim-stylus Plugin
Plugin 'wavded/vim-stylus'

" Git-gutter
Plugin 'airblade/vim-gitgutter'

" vim-surround
Plugin 'tpope/vim-surround'

" Multiple cursors
Plugin 'terryma/vim-multiple-cursors'

" Syntastic
Plugin 'scrooloose/syntastic'

" All of your Plugins must be added before the following line
call vundle#end()            " required
" To plugin indent changes
filetype plugin on    " required

" Font, indent, syntax and coloscheme
if has('gui_running')
  set guifont=Monaco:h16
endif
color Dracula
syntax on
filetype on
filetype indent on

" Line numbers
set number

" Tabs
set ts=4
set softtabstop=4
set shiftwidth=4

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

" Emmet indent
let g:user_emmet_settings = { 
  \  'indentation' : '	', 
  \}

" Fix git-sh-setup in MacVim
if has("gui_macvim")
    set shell=/bin/bash\ -l
endif

" Syntastic stuff
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
