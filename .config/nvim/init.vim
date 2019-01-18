" Init {{{

" Set a env var to detect that we are on neovim's terminal emulator
let $NVIM_TERM = 1

" }}}
" Plug stuff {{{

" Setting up Plug
let plug_vim = expand('~/.config/nvim/autoload/plug.vim')

if !filereadable(plug_vim)
  echo 'Installing Plug...'
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
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
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-ultisnips'
Plug 'ncm2/ncm2-cssomni'
Plug 'ncm2/ncm2-tern', { 'do': 'npm install', 'for': 'javascript.jsx' }
Plug 'mhartington/nvim-typescript', { 'do': './install.sh', 'for': 'typescript' }
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
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }

call plug#end()

" }}}
" Neovim config {{{

let mapleader = ','

syntax on
filetype plugin indent on

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
set completeopt=noinsert,menuone,noselect
set hidden
set listchars=tab:>~,nbsp:_,trail:~
set list
set noshowmode
set colorcolumn=81

let g:onedark_termcolors = 256
colorscheme onedark

" Skip location list and quick fix list on buffer switch and close
augroup skip_qf
  autocmd!
  autocmd FileType qf set nobuflisted
augroup END

" Set fold level to 0 when in vim syntax
augroup vim_fold
  autocmd!
  autocmd Syntax vim setlocal foldmethod=marker foldlevel=0
augroup END

" Check for external file changes
augroup check_changes
  autocmd!
  autocmd FocusGained,BufEnter * :checktime
augroup END

" Close loclist on buffer close
augroup close_loclist
  autocmd!
  autocmd BufDelete,BufLeave * if empty(&buftype) | lclose | endif
augroup END

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

" Why is this not a built-in Vim script function?!
function! s:get_visual_selection()
    let [line_start, column_start] = getpos("'<")[1:2]
    let [line_end, column_end] = getpos("'>")[1:2]
    let lines = getline(line_start, line_end)
    if len(lines) == 0
        return ''
    endif
    let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
    let lines[0] = lines[0][column_start - 1:]
    return join(lines, "\n")
endfunction

" Indent a long javascript object, array or parameter list
function! IndentList()
  let delimiter_map = { '{': '}', '[': ']', '(': ')' }
  let selection = s:get_visual_selection()
  let start_delimiters = join(keys(delimiter_map), '\|')
  let start_delimiter = matchstr(selection, start_delimiters)
  let end_delimiter = delimiter_map[start_delimiter]
  let space = matchstr(getline('v'), '\v^(\s*)')
  let step1 = substitute(
  \ selection,
  \ '\v.{-}\' . start_delimiter . '\s?',
  \ '\="' . start_delimiter . '\r' . space . '  "',
  \ ''
  \ )
  let step2 = substitute(
  \ step1,
  \ '\v.{-},\zs\s?\ze',
  \ '\="\r' . space . '  "',
  \ 'g'
  \ )
  let step3 = substitute(
  \ step2,
  \ '\v.{-}\zs\s?\' . end_delimiter . '(.*\' . end_delimiter . ')@!.*\ze',
  \ '\="\r' . space . '\' . end_delimiter . '"',
  \ ''
  \ )

  silent! execute 's/\v.{-}\zs\' . start_delimiter . '.{-}\' . end_delimiter . '(.*\' . end_delimiter . ')@!\ze/' . step3
endfunction

function! CreateFile()
  let filename = input('New file name: ')
  if strchars(filename) == 0
    return
  endif
  silent! execute '!touch ' . expand('%') . filename
  normal R
endfunction

function! RemoveFileOrDir()
  let current_file = getline('.')
  silent! execute '!rm -rf ' . current_file
  normal R
endfunction

function! CreateDir()
  let dirname = input('New dir name: ')
  if strchars(dirname) == 0
    return
  endif
  silent! execute '!mkdir -p ' . expand('%') . dirname
  normal R
endfunction

function! CopyFileOrDir()
  let current_file = getline('.')
  let destination = input('Copy destination: ', current_file)
  let flag = match(current_file, '/$') > -1 ? ' -R ' : ' '
  if strchars(destination) == 0
    return
  endif
  silent! execute '!cp' . flag . current_file . ' ' . destination
  normal R
endfunction

function! MoveFileOrDir()
  let current_file = getline('.')
  let destination = input('Move destination: ', current_file)
  if strchars(destination) == 0
    return
  endif
  silent! execute '!mv ' . current_file . ' ' . destination
  normal R
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
\ 'javascript': ['eslint'],
\ 'typescript': ['tslint', 'tsserver', 'typecheck'],
\ 'graphql': [],
\ }

" Tsuquyomi config
let g:tsuquyomi_disable_quickfix = 1

" fzf config
let g:rg_command = 'rg --column --line-number --no-heading --ignore-case --hidden --follow --color=always --ignore-file ~/.agignore '
command! -bang -nargs=* Rg call fzf#vim#grep(g:rg_command.shellescape(<q-args>), 1, <bang>0)

" dirvish config
let dirvish_mode = ':sort ,^.*/,'

" vim-prettier config
let g:prettier#autoformat = 0
let g:prettier#quickfix_enabled = 0
let has_prettier_rc = filereadable(fnamemodify('.prettierrc', ':p'))
let has_prettier_js = filereadable(fnamemodify('prettier.config.js', ':p'))
let has_prettier_config = has_prettier_rc || has_prettier_js
if (has_prettier_config)
  augroup prettier
    autocmd!
    autocmd BufWritePre *.js,*.jsx,*.ts,*.tsx,*.css,*.json,*.md Prettier
  augroup END
endif

" ncm2 config
autocmd BufEnter * call ncm2#enable_for_buffer()

" }}}
" Macros {{{

" Macro for visualizing blocks
let @v = 'V$%'

" Macro for navigating blocks
let @n = '$%'

" }}}
" Mappings {{{

" Fold with space
nnoremap <space> za

" Navigate visual lines seamlessly
nnoremap j gj
nnoremap k gk

" Tab switches to the last used buffer
nnoremap <Tab> :b#<CR>

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
nnoremap <Leader>c :bdelete<CR>

" Leader a opens rg
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
vnoremap <Leader>il :call IndentList()<CR>

" Leader ir indents a JSX component
vnoremap <Leader>ir :call IndentReact()<CR>

" Tab to select the popup menu
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Enter closes popup and inserts new line
inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")

augroup dirvish_mappings
  autocmd!
  autocmd FileType dirvish nnoremap <buffer> <Leader>da :call CreateFile()<CR>
  autocmd FileType dirvish nnoremap <buffer> <Leader>dr :call RemoveFileOrDir()<CR>
  autocmd FileType dirvish nnoremap <buffer> <Leader>dmk :call CreateDir()<CR>
  autocmd FileType dirvish nnoremap <buffer> <Leader>dc :call CopyFileOrDir()<CR>
  autocmd FileType dirvish nnoremap <buffer> <Leader>dmv :call MoveFileOrDir()<CR>
augroup END

augroup typescript_mappings
  autocmd!
  autocmd FileType typescript nnoremap <buffer> <Leader>gt :<C-u>echo tsuquyomi#hint()<CR>
  autocmd FileType typescript nnoremap <buffer> <Leader>grn :TsuRenameSymbol<CR>
augroup END

" }}}
