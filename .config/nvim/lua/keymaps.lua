local fns = require('functions')

table.unpack = table.unpack or unpack

local g = vim.g
local opt = vim.opt
local opt_local = vim.opt_local
local fn = vim.fn
local cmd = vim.cmd
local api = vim.api
local env = vim.env
local keymap = vim.keymap

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

-- Leader c deletes current buffer while maintaining the window
keymap.set('n', '<Leader>c', fns.window_safe_buffer_delete)

-- Leader a opens rg
keymap.set('n', '<Leader>a', ':Rg ')
keymap.set('v', '<Leader>a', fns.search_with_current_selection, { silent = true })
keymap.set('n', '<Leader>aw', fns.search_with_current_word, { silent = true })

-- Leader e evaluates current file
keymap.set('n', '<Leader>e', ':source %<CR>')

-- Leader t sets or shows current tab config
keymap.set('n', '<Leader>t', fns.stab)

-- Leader v edits init.lua file
keymap.set('n', '<Leader>v', ':e ~/.config/nvim/init.lua<CR>')

-- Leader w removes trailing whitespace
keymap.set('n', '<Leader>w', [[:%s/\s\+$//g<CR>]])

-- Ctrl+n on normal mode puts the current word on the search register and highlights it
keymap.set('n', '<C-n>', [["zyiw :let @/=''.@z.''<CR> viw]])

-- Ctrl+n on visual mode puts the current selection on the search register and highlights it
keymap.set('v', '<C-n>', [["zy :let @/=''.@z.''<CR> gv]])

-- Leader s sorts range by length
keymap.set('v', '<Leader>s', fns.sort_lines, { silent = true })

-- Leader ii indents a import statement
keymap.set('v', '<Leader>il', fns.indent_list, { silent = true })

-- Leader ir indents a JSX component
keymap.set('v', '<Leader>ir', fns.indent_react, { silent = true })

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
    keymap.set('n', '<Leader>da', fns.create_file_or_dir, { buffer = true, silent = true })
    keymap.set('n', '<Leader>dr', fns.remove_file_or_dir, { buffer = true, silent = true })
    keymap.set('n', '<Leader>dc', fns.copy_file_or_dir, { buffer = true, silent = true })
    keymap.set('n', '<Leader>dm', fns.move_file_or_dir, { buffer = true, silent = true })
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
    keymap.set('n', 'pp', function() fns.push_git_branch_to_origin(false, false) end, { buffer = true, silent = true })
    keymap.set('n', 'pn', function() fns.push_git_branch_to_origin(true, false) end, { buffer = true, silent = true })
    keymap.set('n', 'pf', function() fns.push_git_branch_to_origin(false, true) end, { buffer = true, silent = true })
    keymap.set('n', 'pnf', function() fns.push_git_branch_to_origin(true, true) end, { buffer = true, silent = true })
    keymap.set('n', 'pl', ':<C-U>Git --paginate pl<CR>', { buffer = true, silent = true })
  end
})

api.nvim_create_autocmd('FileType', {
  pattern = 'GV',
  group = api.nvim_create_augroup('gv_mappings', { clear = true }),
  callback = function()
    keymap.set('n', 'cf', fns.commit_fixup_to_current_sha, { buffer = true, silent = true })
    keymap.set('n', 'ri', fns.interactive_rebase_with_current_sha, { buffer = true, silent = true })
    keymap.set('n', 'rf', fns.auto_squash_rebase_with_current_sha, { buffer = true, silent = true })
  end
})