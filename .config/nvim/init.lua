-- Init
require('plugins')

table.unpack = table.unpack or unpack

local g = vim.g
local opt = vim.opt
local opt_local = vim.opt_local
local fn = vim.fn
local cmd = vim.cmd
local api = vim.api
local env = vim.env
local keymap = vim.keymap

-- Set a env var to detect that we are on neovim's terminal emulator
env.NVIM_TERM = 1

-- Neovim config

g.mapleader = ','

cmd.syntax('on')
opt.autoindent = true
cmd.filetype('plugin indent on')

opt.termguicolors = true
opt.hlsearch = false
opt.number = true
opt.cursorline = true
opt.foldenable = true
opt.foldlevelstart = 99
opt.foldnestmax = 10
opt.foldmethod = 'indent'
opt.backup = true
opt.writebackup = true
opt.backupcopy = 'yes'
opt.backupdir = vim.env.HOME .. '/.nvim/tmp'
opt.backupskip = { '/tmp/*', '/private/tmp/*' }
opt.directory = vim.env.HOME .. '/.nvim/tmp'
opt.undofile = true
opt.undodir = vim.env.HOME .. '/.nvim/undo'
opt.undolevels = 1000
opt.undoreload = 10000
opt.shada = [['20,<1000,s1000]]
opt.completeopt = { 'noinsert', 'menuone', 'noselect' }
opt.hidden = true
opt.showbreak = [[↪\ ]]
opt.listchars = { tab = [[»\ ]], nbsp = '•', trail = '•', extends = '›', precedes = '‹' }
opt.list = true
opt.showmode = false
opt.colorcolumn = '81'

opt.background = 'dark'
cmd.colorscheme('palenight')

-- Customize diagnostics colors to match palenight theme
cmd.highlight('DiagnosticError guifg=' .. fn['palenight#GetColors']().red.gui)
cmd.highlight('DiagnosticWarn guifg=' .. fn['palenight#GetColors']().yellow.gui)
cmd.highlight('DiagnosticHint guifg=' .. fn['palenight#GetColors']().white.gui)

-- Disable underline on diganostics
vim.diagnostic.config({ underline = false })

-- Skip location list and quick fix list on buffer switch and close
api.nvim_create_autocmd('FileType', {
  pattern = 'qf',
  group = api.nvim_create_augroup('skip_qf', { clear = true }),
  command = 'set nobuflisted'
})

-- Set fold level to 0 when in vim syntax
api.nvim_create_autocmd('Syntax', {
  pattern = 'vim',
  group = api.nvim_create_augroup('vim_fold', { clear = true }),
  command = 'setlocal foldmethod=marker foldlevel=0'
})

-- Check for external file changes
api.nvim_create_autocmd({ 'FocusGained', 'BufEnter' }, {
  pattern = '*',
  group = api.nvim_create_augroup('check_changes', { clear = true }),
  command = ':checktime'
})

-- Close loclist on buffer close
api.nvim_create_autocmd({ 'BufDelete', 'BufWinLeave' }, {
  pattern = '*',
  group = api.nvim_create_augroup('close_loclist', { clear = true }),
  command = 'if empty(&buftype) | lclose | endif'
})

-- Functions

-- Set tabstop, softtabstop and shiftwidth to the same value
local function summarize_tabs()
  local tabs = {
    tabstop = vim.opt_local.tabstop:get(),
    shiftwidth = vim.opt_local.shiftwidth:get(),
    softtabstop = vim.opt_local.softtabstop:get(),
    expandtab = tostring(vim.opt_local.expandtab:get())
  }
  local message = string.gsub('tabstop = $tabstop, shiftwidth = $shiftwidth, softtabstop = $softtabstop, expandtab = $expandtab', '%$(%w+)', tabs)
  print(message)
end

local function stab()
  local tabstop = tonumber(fn.input('set tabstop = softtabstop = shiftwidth = '))
  if tabstop ~= nil and tabstop > 0 then
    opt_local.softtabstop = tabstop
    opt_local.tabstop = tabstop
    opt_local.shiftwidth = tabstop
  end
  summarize_tabs()
end

-- Organize range by length
local function sort_lines()
  local first_line = fn.getpos('v')[2]
  local last_line = fn.getpos('.')[2]
  cmd([[silent! execute ']] .. first_line .. ',' .. last_line .. [[s/^\(.*\)$/\=strdisplaywidth(submatch(0)) . " " . submatch(0)/']])
  cmd([[silent! execute ']] .. first_line .. ',' .. last_line .. [[sort n']])
  cmd([[silent! execute ']] .. first_line .. ',' .. last_line .. [[s/^\d\+\s//']])
end

-- Indent a React component's jsx code
local function indent_react()
  cmd([[silent! execute 's/\v\<\w+\zs\s\ze|\zs\s\ze\w+\=|("|})\zs\s\ze\w+/\="\n" . matchstr(getline("."), ''^\s*'') . "  "/g']])
  cmd([[silent! execute 's/\v\s?(\/?\>)/\="\n" . matchstr(getline("."), ''^\s*'') . submatch(1)/']])
  cmd.normal('<<')
  cmd([[silent! execute 's/\v\zs(\>)\ze.+/\=submatch(1) . "\n" . matchstr(getline("."), ''^\s*'')/']])
  cmd.normal('>>')
  cmd([[silent! execute 's/\v(\<\/\w+\>)$/\="\n" . matchstr(getline("."), ''^\s*'') . submatch(1)/']])
  cmd.normal('<<')
end

-- Why is this not a built-in Vim script function?!
local function get_visual_selection()
  local visual_start = fn.getpos('v')
  local line_start = visual_start[2]
  local column_start = visual_start[3]
  local visual_end = fn.getpos('.')
  local line_end = visual_end[2]
  local column_end = visual_end[3]
  local lines = fn.getline(line_start, line_end)
  if fn.len(lines) == 0 then
      return ''
  end
  if opt.selection:get() == 'inclusive' then
    lines[#lines] = string.sub(lines[#lines], 1, column_end)
  else
    lines[#lines] = string.sub(lines[#lines], 1, column_end - 1)
  end
  lines[1] = string.sub(lines[1], column_start - 1)
  return fn.join(lines, [[\n]])
end

-- Indent a long javascript object, array or parameter list
local function indent_list()
  local delimiter_map = { ['{'] = '}', ['['] = ']', ['('] = ')' }
  local selection = get_visual_selection()
  local start_delimiters = fn.join(fn.keys(delimiter_map), [[\|]])
  local start_delimiter = fn.matchstr(selection, start_delimiters)
  local end_delimiter = delimiter_map[start_delimiter]
  local space = fn.matchstr(fn.getline('v'), [[\v^(\s*)]])
  local step1 = fn.substitute(
   selection,
   [[\v.{-}\]] .. start_delimiter .. [[\s?]],
   [[\="]] .. start_delimiter .. [[\r]] .. space .. [[  "]],
   ''
  )
  local step2 = fn.substitute(
    step1,
    [[\v.{-},\zs\s?\ze]],
    [[\="\r]] .. space .. [[  "]],
    'g'
  )
  local step3 = fn.substitute(
    step2,
    [[\v.{-}\zs\s?\]] .. end_delimiter .. [[(.*\]] .. end_delimiter .. [[)@!.*\ze]],
    [[\="\r]] .. space .. [[\]] .. end_delimiter .. [["]],
    ''
  )

  cmd([[silent! execute 's/\v.{-}\zs\]] .. start_delimiter .. [[.{-}\]] .. end_delimiter .. [[(.*\]] .. end_delimiter .. [[)@!\ze/]] .. step3 .. [[']])
end

local function create_file_or_dir()
  local file_or_dir = fn.input('New file or dir name: ')
  if #file_or_dir == 0 then
    return
  end
  if string.sub(file_or_dir, -1) == '/' then
    cmd([[silent! execute '!mkdir -p ]] .. fn.expand('%') .. file_or_dir .. [[']])
  else
    cmd([[silent! execute '!touch ]] .. fn.expand('%') .. file_or_dir .. [[']])
  end
  cmd.normal('R')
end

local function remove_file_or_dir()
  local current_file = fn.getline('.')
  cmd([[silent! execute '!rm -rf ]] .. current_file .. [[']])
  cmd.normal('R')
end

local function copy_file_or_dir()
  local current_file = fn.getline('.')
  local destination = fn.input('Copy destination: ', current_file)
  local flag
  if fn.match(current_file, [[/$]]) > -1 then
    flag = ' -R '
  else
    flag = ' '
  end
  if fn.strchars(destination) == 0 then
    return
  end
  cmd([[silent! execute '!cp]] .. flag .. current_file .. ' ' .. destination .. [[']])
  cmd.normal('R')
end

local function move_file_or_dir()
  local current_file = fn.getline('.')
  local destination = fn.input('Move destination: ', current_file)
  if fn.strchars(destination) == 0 then
    return
  end
  cmd([[silent! execute '!mv ]] .. current_file .. ' ' .. destination .. [[']])
  cmd.normal('R')
end

local function push_git_branch_to_origin(no_verify, force)
  local current_branch = fn.system([[git branch | grep -e '^*' | tr -d '*' | tr -cd '[:print:]']])
  if not no_verify and not force then
    cmd.execute('Git --paginate ps -u ' .. current_branch)
  elseif no_verify and not force then
    cmd.execute('Git --paginate ps -u --no-verify ' .. current_branch)
  elseif not no_verify and force then
    fn.execute('Git --paginate ps -u --force ' .. current_branch)
  elseif no_verify and force then
    fn.execute('Git --paginate ps -u --no-verify --force ' .. current_branch)
  end
end

local function refresh_gv()
  fn.execute('bdelete')
  fn.execute('GV')
end

local function commit_fixup_to_current_sha()
  fn.execute('Git commit --fixup=' .. fn['gv#sha']())
  refresh_gv()
end

local function interactive_rebase_with_current_sha()
  fn.execute('Git rebase -i ' .. fn['gv#sha']())
  refresh_gv()
end

local function auto_squash_rebase_with_current_sha()
  fn.execute('Git rebase --autosquash ' .. fn['gv#sha']())
  refresh_gv()
end

local function search_with_current_word()
  fn.execute('Rg ' .. fn.expand('<cword>'))
end

local function search_with_current_selection()
  local current_selection = get_visual_selection()
  fn.execute('Rg ' .. current_selection)
end

-- Plugins config

-- lightline config
g.lightline = {
  colorscheme = 'palenight',
  active = {
    right = {
      { 'lineinfo' },
      { 'percent' },
      { 'filetype' }
    }
  },
  mode_map = {
    n = 'N',
    i = 'I',
    R = 'R',
    v = 'V',
    V = 'V',
    ['\\<C-v>'] = 'V',
    c = 'C',
    s = 'S',
    S = 'S',
    ['\\<C-s>'] = 'S',
    t = 'T'
  }
}

-- UltiSnips bindings
g.UltiSnipsSnippetsDir = '~/.config/nvim/UltiSnips'
g.UltiSnipsExpandTrigger = '<c-j>'

-- Emmet config
g.user_emmet_settings = {
  ['javascript.jsx'] = { extends = 'jsx' }
}

-- fzf config
g.fzf_layout = { down = '40%' }
g.rg_command = 'rg --fixed-strings --column --line-number --no-heading --ignore-case --hidden --follow --color=always --ignore-file ~/.agignore '
api.nvim_create_user_command(
  'Rg',
  'call fzf#vim#grep(g:rg_command.shellescape(<q-args>), 1, <bang>0)',
  { bang = true, nargs = '*' }
)

-- dirvish config
g.dirvish_mode = ':sort ,^.*/,'

-- vim-prettier config
g['prettier#autoformat'] = 0
g['prettier#quickfix_enabled'] = 0
local has_prettier_rc = fn.filereadable(fn.fnamemodify('.prettierrc', ':p'))
local has_prettier_js = fn.filereadable(fn.fnamemodify('prettier.config.js', ':p'))
local has_prettier_config = has_prettier_rc or has_prettier_js
if has_prettier_config then
  api.nvim_create_autocmd('BufWritePre', {
    pattern = { '*.js', '*.jsx', '*.ts', '*.tsx', '*.css', '*.json', '*.md', '*.html' },
    group = api.nvim_create_augroup('prettier', { clear = true }),
    command = 'Prettier'
  })
end

-- fugitive config
api.nvim_create_autocmd('BufReadPost', {
  pattern = 'fugitive://*',
  group = api.nvim_create_augroup('custom_fugitive', { clear = true }),
  command = 'set bufhidden=delete'
})

-- nvim-lint config
require('lint').linters_by_ft = {
  javascript = { 'eslint' },
  typescript = { 'eslint' },
  javascriptreact = { 'eslint' },
  typescriptreact = { 'eslint' },
}

api.nvim_create_autocmd({ 'BufEnter', 'BufWritePost' }, {
  pattern = '*',
  group = api.nvim_create_augroup('nvim_lint', { clear = true }),
  callback = function()
    require('lint').try_lint()
  end
})

-- Macros

-- Macro for visualizing blocks
fn.setreg('v', 'V$%')

-- Macro for navigating blocks
fn.setreg('n', '$%')

-- Mappings

-- Fold with space
keymap.set('n', '<Space>', 'za')

-- Navigate visual lines seamlessly
keymap.set('n', 'j', 'gj')
keymap.set('n', 'k', 'gk')

-- Tab switches to the last used buffer
keymap.set('n', '<Tab>', ':b#<CR>')

-- Yankstack config
keymap.set('n', '<C-j>', '<Plug>yankstack_substitute_older_paste', { silent = true })
keymap.set('n', '<C-k>', '<Plug>yankstack_substitute_newer_paste', { silent = true })

-- Ctrl+p uses fzf
keymap.set('n', '<C-p>', ':FZF<CR>')

-- Leader b uses fzf buffers
keymap.set('n', '<Leader>b', ':Buffers<CR>')

-- Leader y yanks to the plus register
keymap.set({ 'n', 'v' }, '<Leader>y', '"+y')
keymap.set('n', '<Leader>Y', '"+Y')

-- Leader p pastes from the plus register
keymap.set({ 'n', 'v' }, '<Leader>p', '"+p')
keymap.set('n', '<Leader>P', '"+P')

-- Leader n toggles netrw or dirvish if installed
keymap.set('n', '<Leader>n', ':e.<CR>')

-- Leader c deletes current buffer
keymap.set('n', '<Leader>c', ':bdelete<CR>')

-- Leader a opens rg
keymap.set('n', '<Leader>a', ':Rg ')
keymap.set('v', '<Leader>a', search_with_current_selection, { silent = true })
keymap.set('n', '<Leader>aw', search_with_current_word, { silent = true })

-- Leader e evaluates current file
keymap.set('n', '<Leader>e', ':source %<CR>')

-- Leader t sets or shows current tab config
keymap.set('n', '<Leader>t', stab)

-- Leader v edits init.lua file
keymap.set('n', '<Leader>v', ':e ~/.config/nvim/init.lua<CR>')

-- Leader w removes trailing whitespace
keymap.set('n', '<Leader>w', [[:%s/\s\+$//g<CR>]])

-- Ctrl+n on normal mode puts the current word on the search register and highlights it
keymap.set('n', '<C-n>', [["zyiw :let @/=''.@z.''<CR> viw]])

-- Ctrl+n on visual mode puts the current selection on the search register and highlights it
keymap.set('v', '<C-n>', [["zy :let @/=''.@z.''<CR> gv]])

-- Leader s sorts range by length
keymap.set('v', '<Leader>s', sort_lines, { silent = true })

-- Leader ii indents a import statement
keymap.set('v', '<Leader>il', indent_list, { silent = true })

-- Leader ir indents a JSX component
keymap.set('v', '<Leader>ir', indent_react, { silent = true })

-- Leader g opens fugitive status window
keymap.set('n', '<Leader>g', ':Git<CR>')

-- Leader gv opens gv window
keymap.set({ 'n', 'v' }, '<Leader>gv', ':GV<CR>')

-- Tab to select the popup menu
keymap.set('i', '<Tab>', function()
  if fn.pumvisible() ~= 0 then
    return '<C-n>'
  else
    return '<Tab>'
  end
end, { silent = true, expr = true })

keymap.set('i', '<S-Tab>', function()
  if fn.pumvisible() ~= 0 then
    return '<C-p>'
  else
    return '<S-Tab>'
  end
end, { silent = true, expr = true })

-- Enter closes popup and inserts new line
keymap.set('i', '<CR>', function()
  if fn.pumvisible() ~= 0 then
    return '<C-y><CR>'
  else
    return '<CR>'
  end
end, { silent = true, expr = true })

api.nvim_create_autocmd('FileType', {
  pattern = 'dirvish',
  group = api.nvim_create_augroup('dirvish_mappings', { clear = true }),
  callback = function()
    keymap.set('n', '<Leader>da', create_file_or_dir, { buffer = true, silent = true })
    keymap.set('n', '<Leader>dr', remove_file_or_dir, { buffer = true, silent = true })
    keymap.set('n', '<Leader>dc', copy_file_or_dir, { buffer = true, silent = true })
    keymap.set('n', '<Leader>dm', move_file_or_dir, { buffer = true, silent = true })
  end
})

api.nvim_create_autocmd('FileType', {
  pattern = { 'javascript', 'javascript.jsx', 'typescript', 'typescript.tsx', 'typescriptreact' },
  group = api.nvim_create_augroup('javascript_mappings', { clear = true }),
  callback = function()
    keymap.set('n', '<Leader>rn', vim.lsp.buf.rename, { buffer = true, silent = true })
    keymap.set('n', '<Leader>gd', vim.lsp.buf.definition, { buffer = true, silent = true })
    keymap.set('n', '<Leader>gt', vim.lsp.buf.hover, { buffer = true, silent = true })
    keymap.set('n', '<Leader>gr', vim.lsp.buf.references, { buffer = true, silent = true })
  end
})

api.nvim_create_autocmd('TermOpen', {
  pattern = '*',
  group = api.nvim_create_augroup('terminal_mappings', { clear = true }),
  callback = function()
    keymap.set('n', '<Leader>c', ':bdelete!<CR>', { buffer = true })
  end
})

api.nvim_create_autocmd('FileType', {
  pattern = 'fugitive',
  group = api.nvim_create_augroup('fugitive_mappings', { clear = true }),
  callback = function()
    keymap.set('n', 'cn', ':<C-U>Git commit --no-verify<CR>', { buffer = true, silent = true })
    keymap.set('n', 'can', ':<C-U>Git commit --amend --no-verify<CR>', { buffer = true, silent = true })
    keymap.set('n', 'pp', function() push_git_branch_to_origin(false, false) end, { buffer = true, silent = true })
    keymap.set('n', 'pn', function() push_git_branch_to_origin(true, false) end, { buffer = true, silent = true })
    keymap.set('n', 'pf', function() push_git_branch_to_origin(false, true) end, { buffer = true, silent = true })
    keymap.set('n', 'pnf', function() push_git_branch_to_origin(true, true) end, { buffer = true, silent = true })
    keymap.set('n', 'pl', ':<C-U>Git --paginate pl<CR>', { buffer = true, silent = true })
  end
})

api.nvim_create_autocmd('FileType', {
  pattern = 'GV',
  group = api.nvim_create_augroup('gv_mappings', { clear = true }),
  callback = function()
    keymap.set('n', 'cf', commit_fixup_to_current_sha, { buffer = true, silent = true })
    keymap.set('n', 'ri', interactive_rebase_with_current_sha, { buffer = true, silent = true })
    keymap.set('n', 'rf', auto_squash_rebase_with_current_sha, { buffer = true, silent = true })
  end
})
