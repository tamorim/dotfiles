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

" Solarized colorscheme
Plugin 'altercation/vim-colors-solarized'

" Atom dark colorscheme
Plugin 'gosukiwi/vim-atom-dark'

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

" Syntastic
Plugin 'scrooloose/syntastic'

" Vim Sleuth
Plugin 'tpope/vim-sleuth'

" EasyMotion
Plugin 'vim-scripts/EasyMotion'

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
		set guioptions-=r  "remove right-hand scroll bar
		set guioptions-=L  "remove left-hand scroll bar
	endif
endif
syntax on
color atom-dark
filetype on
filetype indent on

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
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup

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

" Increase vim yank buffer line limit and size
set viminfo='20,<1000,s1000
