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

Plug 'dense-analysis/ale'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-unimpaired'
Plug 'mattn/emmet-vim'
Plug 'itchyny/lightline.vim'
Plug 'Raimondi/delimitMate'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-sleuth'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-repeat'
Plug 'SirVer/ultisnips'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'sheerun/vim-polyglot'
Plug 'jparise/vim-graphql'
Plug 'editorconfig/editorconfig-vim'
Plug 'matze/vim-move'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'thomasfaingnaert/vim-lsp-snippets'
Plug 'thomasfaingnaert/vim-lsp-ultisnips'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'lighttiger2505/deoplete-vim-lsp'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'tpope/vim-surround'
Plug 'shime/vim-livedown', { 'for': 'markdown' }
Plug 'osyo-manga/vim-over'
Plug 'justinmk/vim-dirvish'
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }

call plug#end()

" }}}
" Neovim config {{{

let mapleader = ','

syntax on
set autoindent
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
set writebackup
set backupcopy=yes
set backupdir=$HOME/.nvim/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=$HOME/.nvim/tmp
set undofile
set undodir=$HOME/.nvim/undo
set undolevels=1000
set undoreload=10000
set viminfo='20,<1000,s1000
set completeopt=noinsert,menuone,noselect
set hidden
set showbreak=↪\ 
set listchars=tab:»\ ,nbsp:•,trail:•,extends:›,precedes:‹
set list
set noshowmode
set colorcolumn=81

set background=dark
colorscheme palenight

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
  autocmd BufDelete,BufWinLeave * if empty(&buftype) | lclose | endif
augroup END

" }}}
" Functions {{{

" Set tabstop, softtabstop and shiftwidth to the same value
command! -nargs=* Stab call Stab()
function! Stab() abort
  let l:tabstop = 1 * input('set tabstop = softtabstop = shiftwidth = ')
  if l:tabstop > 0
    let &l:sts = l:tabstop
    let &l:ts = l:tabstop
    let &l:sw = l:tabstop
  endif
  call SummarizeTabs()
endfunction

function! SummarizeTabs() abort
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
function! SortLines() range abort
  silent! execute a:firstline . ',' . a:lastline . 's/^\(.*\)$/\=strdisplaywidth(submatch(0)) . " " . submatch(0)/'
  silent! execute a:firstline . ',' . a:lastline . 'sort n'
  silent! execute a:firstline . ',' . a:lastline . 's/^\d\+\s//'
endfunction

" Indent a React component's jsx code
function! IndentReact() abort
  silent! execute 's/\v\<\w+\zs\s\ze|\zs\s\ze\w+\=|("|})\zs\s\ze\w+/\="\n" . matchstr(getline("."), ''^\s*'') . "  "/g'
  silent! execute 's/\v\s?(\/?\>)/\="\n" . matchstr(getline("."), ''^\s*'') . submatch(1)/'
  normal <<
  silent! execute 's/\v\zs(\>)\ze.+/\=submatch(1) . "\n" . matchstr(getline("."), ''^\s*'')/'
  normal >>
  silent! execute 's/\v(\<\/\w+\>)$/\="\n" . matchstr(getline("."), ''^\s*'') . submatch(1)/'
  normal <<
endfunction

" Why is this not a built-in Vim script function?!
function! s:get_visual_selection() abort
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
function! IndentList() abort
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

function! CreateFileOrDir() abort
  let file_or_dir = input('New file or dir name: ')
  let length = strchars(file_or_dir)
  if length == 0
    return
  endif
  if file_or_dir[length - 1] == '/'
    silent! execute '!mkdir -p ' . expand('%') . file_or_dir
  else
    silent! execute '!touch ' . expand('%') . file_or_dir
  endif
  normal R
endfunction

function! RemoveFileOrDir() abort
  let current_file = getline('.')
  silent! execute '!rm -rf ' . current_file
  normal R
endfunction

function! CopyFileOrDir() abort
  let current_file = getline('.')
  let destination = input('Copy destination: ', current_file)
  let flag = match(current_file, '/$') > -1 ? ' -R ' : ' '
  if strchars(destination) == 0
    return
  endif
  silent! execute '!cp' . flag . current_file . ' ' . destination
  normal R
endfunction

function! MoveFileOrDir() abort
  let current_file = getline('.')
  let destination = input('Move destination: ', current_file)
  if strchars(destination) == 0
    return
  endif
  silent! execute '!mv ' . current_file . ' ' . destination
  normal R
endfunction

function! PushGitBranchToOrigin(no_verify, force) abort
  let current_branch = system("git branch | grep -e '^*' | tr -d '*' | tr -cd '[:print:]'")
  if !a:no_verify && !a:force
    execute 'Git --paginate ps -u ' . current_branch
  elseif a:no_verify && !a:force
    execute 'Git --paginate ps -u --no-verify ' . current_branch
  elseif !a:no_verify && a:force
    execute 'Git --paginate ps -u --force ' . current_branch
  elseif a:no_verify && a:force
    execute 'Git --paginate ps -u --no-verify --force ' . current_branch
  endif
endfunction

function! RefreshGV() abort
  execute 'bdelete'
  execute 'GV'
endfunction

function! CommitFixupToCurrentSha() abort
  execute 'Gcommit --fixup=' . gv#sha()
  call RefreshGV()
endfunction

function! InteractiveRebaseWithCurrentSha() abort
  execute 'Git rebase -i ' . gv#sha()
  call RefreshGV()
endfunction

function! AutoSquashRebaseWithCurrentSha() abort
  execute 'Grebase --autosquash ' . gv#sha()
  call RefreshGV()
endfunction

function! SearchWithCurrentWord() abort
  execute 'Rg ' . expand('<cword>')
endfunction

function! SearchWithCurrentSelection() abort
  let current_selection = s:get_visual_selection()
  execute 'Rg ' . current_selection
endfunction

" }}}
" Plugins config {{{

" lightline config
let g:lightline = {
\ 'colorscheme': 'palenight',
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
let g:ale_disable_lsp = 1
let g:ale_open_list = 1
let g:ale_list_window_size = 5
let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_insert_leave = 0
let g:ale_linters = {
\ 'javascript': ['eslint'],
\ 'typescript': ['tslint', 'tsserver'],
\ 'graphql': [],
\ }

" fzf config
let g:rg_command = 'rg --fixed-strings --column --line-number --no-heading --ignore-case --hidden --follow --color=always --ignore-file ~/.agignore '
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
    autocmd BufWritePre *.js,*.jsx,*.ts,*.tsx,*.css,*.json,*.md,*.html Prettier
  augroup END
endif

" fugitive config
augroup custom_fugitive
  autocmd!
  autocmd BufReadPost fugitive://* set bufhidden=delete
augroup END

" polyglot config
let g:polyglot_disabled = ['jsx']

" deoplete config
let g:deoplete#enable_at_startup = 1

" lsp config
let g:lsp_diagnostics_echo_cursor = 1
let g:lsp_virtual_text_enabled = 0

" }}}
" Macros {{{

" Macro for visualizing blocks
let @v = 'V$%'

" Macro for navigating blocks
let @n = '$%'

" }}}
" Mappings {{{

" Fold with space
nnoremap <Space> za

" Navigate visual lines seamlessly
nnoremap j gj
nnoremap k gk

" Tab switches to the last used buffer
nnoremap <Tab> :b#<CR>

" Yankstack config
nmap <silent> <C-j> <Plug>yankstack_substitute_older_paste
nmap <silent> <C-k> <Plug>yankstack_substitute_newer_paste

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
vnoremap <silent> <Leader>a :call SearchWithCurrentSelection()<CR>
nnoremap <silent> <Leader>aw :call SearchWithCurrentWord()<CR>

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
vnoremap <silent> <Leader>s :call SortLines()<CR>

" Leader ii indents a import statement
vnoremap <silent> <Leader>il :call IndentList()<CR>

" Leader ir indents a JSX component
vnoremap <silent> <Leader>ir :call IndentReact()<CR>

" Leader g opens fugitive status window
nnoremap <Leader>g :Gstatus<CR>

" Leader gv opens gv window
nnoremap <Leader>gv :GV<CR>
vnoremap <Leader>gv :GV<CR>

" Tab to select the popup menu
inoremap <silent> <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <silent> <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Enter closes popup and inserts new line
inoremap <silent> <expr> <CR> (pumvisible() ? "\<C-y>\<CR>" : "\<CR>")

augroup dirvish_mappings
  autocmd!
  autocmd FileType dirvish nnoremap <buffer> <silent> <Leader>da :call CreateFileOrDir()<CR>
  autocmd FileType dirvish nnoremap <buffer> <silent> <Leader>dr :call RemoveFileOrDir()<CR>
  autocmd FileType dirvish nnoremap <buffer> <silent> <Leader>dc :call CopyFileOrDir()<CR>
  autocmd FileType dirvish nnoremap <buffer> <silent> <Leader>dm :call MoveFileOrDir()<CR>
augroup END

augroup javascript_mappings
  autocmd!
  autocmd FileType javascript,javascript.jsx,typescript,typescript.tsx,typescriptreact nmap <buffer> <silent> <Leader>rn :LspRename<CR>
  autocmd FileType javascript,javascript.jsx,typescript,typescript.tsx,typescriptreact nmap <buffer> <silent> <C-]> :LspDefinition<CR>
  autocmd FileType javascript,javascript.jsx,typescript,typescript.tsx,typescriptreact nmap <buffer> <silent> <Leader>gt :LspHover<CR>
augroup END

augroup terminal_mappings
  autocmd!
  autocmd TermOpen * nnoremap <buffer> <Leader>c :bdelete!<CR>
augroup END

augroup fugitive_mappings
  autocmd!
  autocmd FileType fugitive nnoremap <buffer> <silent> cn :<C-U>Gcommit --no-verify<CR>
  autocmd FileType fugitive nnoremap <buffer> <silent> can :<C-U>Gcommit --amend --no-verify<CR>
  autocmd FileType fugitive nnoremap <buffer> <silent> pp :<C-U>call PushGitBranchToOrigin(0, 0)<CR>
  autocmd FileType fugitive nnoremap <buffer> <silent> pn :<C-U>call PushGitBranchToOrigin(1, 0)<CR>
  autocmd FileType fugitive nnoremap <buffer> <silent> pf :<C-U>call PushGitBranchToOrigin(0, 1)<CR>
  autocmd FileType fugitive nnoremap <buffer> <silent> pnf :<C-U>call PushGitBranchToOrigin(1, 1)<CR>
augroup END

augroup gv_mappings
  autocmd!
  autocmd FileType GV nnoremap <buffer> <silent> cf :<C-U>call CommitFixupToCurrentSha()<CR>
  autocmd FileType GV nnoremap <buffer> <silent> ri :<C-U>call InteractiveRebaseWithCurrentSha()<CR>
  autocmd FileType GV nnoremap <buffer> <silent> rf :<C-U>call AutoSquashRebaseWithCurrentSha()<CR>
augroup END

" }}}
