table.unpack = table.unpack or unpack

local g = vim.g
local opt = vim.opt
local fn = vim.fn
local cmd = vim.cmd
local api = vim.api
local env = vim.env

-- Set a env var to detect that we are on neovim's terminal emulator
env.NVIM_TERM = 1

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
opt.completeopt = { 'menu', 'menuone', 'noinsert', 'noselect' }
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
  command = 'set nobuflisted',
})

-- Set fold level to 0 when in vim syntax
api.nvim_create_autocmd('Syntax', {
  pattern = 'vim',
  group = api.nvim_create_augroup('vim_fold', { clear = true }),
  command = 'setlocal foldmethod=marker foldlevel=0',
})

-- Check for external file changes
api.nvim_create_autocmd({ 'FocusGained', 'BufEnter' }, {
  pattern = '*',
  group = api.nvim_create_augroup('check_changes', { clear = true }),
  command = ':checktime',
})

-- Close loclist on buffer close
api.nvim_create_autocmd({ 'BufDelete', 'BufWinLeave' }, {
  pattern = '*',
  group = api.nvim_create_augroup('close_loclist', { clear = true }),
  command = 'if empty(&buftype) | lclose | endif',
})
