" Init {{{

" Set a env var to detect that we are on neovim's terminal emulator
let $NVIM_TERM = 1

" }}}
" Plug stuff {{{

" Setting up Plug
let isPlugPresent = 1
let plug_vim = expand('~/.config/nvim/autoload/plug.vim')

if !filereadable(plug_vim)
  echo 'Installing Plug...'
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  let isPlugPresent = 0
endif

if isPlugPresent == 0
  echo 'Installing plugins...'
  echo ''
  silent! source ~/.config/nvim/init.vim
  silent! PlugInstall
  silent! bdelete
endif

call plug#begin()

Plug 'w0rp/ale'
Plug 'tpope/vim-fugitive'
Plug 'mattn/emmet-vim'
Plug 'itchyny/lightline.vim'
Plug 'Raimondi/delimitMate'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-sleuth'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-repeat'
Plug 'SirVer/ultisnips'
Plug 'sheerun/vim-polyglot'
Plug 'editorconfig/editorconfig-vim'
Plug 'matze/vim-move'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --js-completer' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'ternjs/tern_for_vim', { 'do': 'npm i', 'for': 'javascript.jsx' }
Plug 'joshdick/onedark.vim'
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'tpope/vim-surround'
Plug 'shime/vim-livedown', { 'for': 'markdown' }
Plug 'osyo-manga/vim-over'
Plug 'justinmk/vim-dirvish'
Plug 'Shougo/vimproc.vim', { 'do': 'make', 'for': 'typescript' }
Plug 'Quramy/tsuquyomi', { 'for': 'typescript' }

call plug#end()

" }}}
" Neovim config {{{

let mapleader = ','

syntax on
filetype on
filetype indent on

set termguicolors
set nohlsearch
set number
set cursorline
set foldenable
set foldlevelstart=99
set foldnestmax=10
set foldmethod=indent
set backup
set backupcopy=yes
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup
set undofile
set undodir=$HOME/.nvim/undo
set undolevels=1000
set undoreload=10000
set viminfo='20,<1000,s1000
set completeopt-=preview
set hidden
set listchars=tab:>~,nbsp:_,trail:~
set list
set noshowmode

let g:onedark_termcolors = 256
colorscheme onedark

" Make the 81st column stand out
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)

" Skip location list and quick fix list on buffer switch and close
augroup qf
  autocmd!
  autocmd FileType qf set nobuflisted
augroup END

autocmd Syntax vim setlocal foldmethod=marker foldlevel=0

" }}}
" Functions {{{

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

" Organize range by length
function! SortLines() range
  silent! execute a:firstline . ',' . a:lastline . 's/^\(.*\)$/\=strdisplaywidth(submatch(0)) . " " . submatch(0)/'
  silent! execute a:firstline . ',' . a:lastline . 'sort n'
  silent! execute a:firstline . ',' . a:lastline . 's/^\d\+\s//'
endfunction

" Indent a React component's jsx code
function! IndentReact()
  silent! execute 's/\v\<\w+\zs\s\ze|\zs\s\ze\w+\=|("|})\zs\s\ze\w+/\="\n" . matchstr(getline("."), ''^\s*'') . "  "/g'
  silent! execute 's/\v\s?(\/?\>)/\="\n" . matchstr(getline("."), ''^\s*'') . submatch(1)/'
  normal <<
  silent! execute 's/\v\zs(\>)\ze.+/\=submatch(1) . "\n" . matchstr(getline("."), ''^\s*'')/'
  normal >>
  silent! execute 's/\v(\<\/\w+\>)$/\="\n" . matchstr(getline("."), ''^\s*'') . submatch(1)/'
  normal <<
endfunction

" Indent a long javascript import statement
function! IndentImport()
  silent! execute 's/\v\{\zs\ze/\="\n" .  "  "/g'
  silent! execute 's/\v\w+,\zs\s?\ze/\="\n" .  "  "/g'
  silent! execute 's/\v\zs\ze\}/\=",\n"/g'
endfunction

" }}}
" Plugins config {{{

" lightline config
let g:lightline = {
\ 'colorscheme': 'one',
\ 'active': {
\   'right': [
\     ['lineinfo'],
\     ['percent'],
\     ['filetype'],
\   ],
\ },
\ 'mode_map': {
\   'n': 'N',
\   'i': 'I',
\   'R': 'R',
\   'v': 'V',
\   'V': 'V',
\   "\<C-v>": 'V',
\   'c': 'C',
\   's': 'S',
\   'S': 'S',
\   "\<C-s>": 'S',
\   't': 'T',
\ },
\ }

" Enable vim-jsx on js files
let g:jsx_ext_required = 0

" UltiSnips bindings
let g:UltiSnipsSnippetsDir = '~/.config/nvim/UltiSnips'
let g:UltiSnipsExpandTrigger = '<c-j>'

" Tern config
let g:tern#command = ['tern']
let g:tern_request_timeout = 1
let g:tern_show_signature_in_pum = 1
let g:tern#arguments = ['--persistent']
let g:tern#filetypes = ['jsx', 'javascript.jsx']

" Emmet config
let g:user_emmet_settings = {
\ 'javascript.jsx': { 'extends': 'jsx' }
\ }

" ALE config
let g:ale_open_list = 1
let g:ale_list_window_size = 5
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters = {
\ 'typescript': ['tslint', 'tsserver', 'typecheck']
\ }

" Tsuquyomi config
let g:tsuquyomi_completion_detail = 1
let g:tsuquyomi_disable_quickfix = 1

" fzf config
let g:rg_command = 'rg --column --line-number --no-heading --ignore-case --hidden --follow --color=always --ignore-file ~/.agignore '
command! -bang -nargs=* Rg call fzf#vim#grep(g:rg_command.shellescape(<q-args>), 1, <bang>0)

" dirvish config
let dirvish_mode = ':sort ,^.*/,'

" }}}
" Macros {{{

" Macro for visualizing blocks
let @v = 'V$%'

" Macro for navigating blocks
let @n = '$%'

" Macro for deleting blocks and a line after
let @f = 'V$%jd'

" Macro for deleting stuff surrounding blocks
let @s = '0$wviB<b%dddd'

" }}}
" Mappings {{{

" Fold with space
nnoremap <space> za

" Navigate visual lines seamlessly
nnoremap j gj
nnoremap k gk

" Yankstack config
nmap <C-j> <Plug>yankstack_substitute_older_paste
nmap <C-k> <Plug>yankstack_substitute_newer_paste

" Ctrl+p uses fzf
nmap <C-p> :FZF<CR>

" Leader b uses fzf buffers
nmap <Leader>b :Buffers<CR>

" Leader y yanks to the plus register
nnoremap <Leader>y "+y
nnoremap <Leader>Y "+Y
vnoremap <Leader>y "+y

" Leader p pastes from the plus register
nnoremap <Leader>p "+p
nnoremap <Leader>P "+P
vnoremap <Leader>p "+p

" Leader n toggles netrw or dirvish if installed
nnoremap <Leader>n :e.<CR>

" Leader c deletes current buffer
nnoremap <Leader>c :lcl \| bdelete<CR>

" Leader a opens ag
nnoremap <Leader>a :Rg 

" Leader e evaluates current file
nnoremap <Leader>e :source %<CR>

" Leader t sets or shows current tab config
nnoremap <Leader>t :Stab<CR>

" Leader v edits init.vim file
nnoremap <Leader>v :e ~/.config/nvim/init.vim<CR>

" Leader w removes trailing whitespace
nnoremap <Leader>w :%s/\s\+$//g<CR>

" Ctrl+n on normal mode puts the current word on the search register and highlights it
nnoremap <C-n> "zyiw :let @/=''.@z.''<CR> viw

" Ctrl+n on visual mode puts the current selection on the search register and highlights it
vnoremap <C-n> "zy :let @/=''.@z.''<CR> gv

" Leader s sorts range by length
vnoremap <Leader>s :call SortLines()<CR>

" Leader ii indents a import statement
vnoremap <Leader>ii :call IndentImport()<CR> kvi{ :call SortLines()<CR>

" Leader ir indents a JSX component
vnoremap <Leader>ir :call IndentReact()<CR>

" }}}
