table.unpack = table.unpack or unpack

local g = vim.g
local opt = vim.opt
local opt_local = vim.opt_local
local fn = vim.fn
local cmd = vim.cmd
local api = vim.api
local env = vim.env
local keymap = vim.keymap

local ensure_packer = function()
  local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path })
    cmd.packadd('packer.nvim')
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

require('packer').startup(function(use)
  use('wbthomason/packer.nvim')

  -- Color scheme and syntax highlight
  use('drewtempelmeyer/palenight.vim')
  use('sheerun/vim-polyglot')
  use('brenoprata10/nvim-highlight-colors')

  -- Git
  use('tpope/vim-fugitive')
  use({ 'tpope/vim-rhubarb', requires = 'tpope/vim-fugitive' })
  use('junegunn/gv.vim')
  use('airblade/vim-gitgutter')

  -- Enhancements
  use('tpope/vim-sleuth')
  use('tpope/vim-surround')
  use('tpope/vim-rsi')
  use('tpope/vim-unimpaired')
  use('tpope/vim-repeat')
  use('Raimondi/delimitMate')
  use('tomtom/tcomment_vim')
  use('maxbrunsfeld/vim-yankstack')
  use('osyo-manga/vim-over')
  use('mattn/emmet-vim')

  -- Completion and lint
  use('neovim/nvim-lspconfig')
  use({ 'hrsh7th/nvim-cmp', requires = {
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-nvim-lsp-signature-help',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline'
  } })
  use('SirVer/ultisnips')
  use({ 'quangnguyen30192/cmp-nvim-ultisnips', requires = {
   'hrsh7th/nvim-cmp',
   'SirVer/ultisnips'
  } })
  use('mfussenegger/nvim-lint')

  -- Formatters
  use('editorconfig/editorconfig-vim')
  use({ 'prettier/vim-prettier', run = 'yarn install' })

  -- Misc
  use({ 'junegunn/fzf.vim', requires = env.FZF_PATH })
  use('justinmk/vim-dirvish')
  use('itchyny/lightline.vim')
  use({ 'shime/vim-livedown', ft = 'markdown' })

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require('packer').sync()
  end
end)

-- Set up nvim-cmp.
local cmp = require('cmp')

cmp.setup({
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ["<Tab>"] = cmp.mapping({
      c = function()
        if cmp.visible() then
          cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
        else
          cmp.complete()
        end
      end,
      i = function(fallback)
        if cmp.visible() then
          cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
        elseif fn["UltiSnips#CanJumpForwards"]() == 1 then
          api.nvim_feedkeys(t("<Plug>(ultisnips_jump_forward)"), 'm', true)
        else
          fallback()
        end
      end,
      s = function(fallback)
        if fn["UltiSnips#CanJumpForwards"]() == 1 then
          api.nvim_feedkeys(t("<Plug>(ultisnips_jump_forward)"), 'm', true)
        else
          fallback()
        end
      end
    }),
    ["<S-Tab>"] = cmp.mapping({
      c = function()
        if cmp.visible() then
          cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
        else
          cmp.complete()
        end
      end,
      i = function(fallback)
        if cmp.visible() then
          cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
        elseif fn["UltiSnips#CanJumpBackwards"]() == 1 then
          return api.nvim_feedkeys( t("<Plug>(ultisnips_jump_backward)"), 'm', true)
        else
          fallback()
        end
      end,
      s = function(fallback)
        if fn["UltiSnips#CanJumpBackwards"]() == 1 then
          return api.nvim_feedkeys( t("<Plug>(ultisnips_jump_backward)"), 'm', true)
        else
          fallback()
        end
      end
    }),
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'nvim_lsp_signature_help' },
    { name = 'ultisnips' }, -- For ultisnips users.
  }, {
    { name = 'buffer' },
    { name = 'path' },
  }),
  view = {
    entries = { name = 'custom', selection_order = 'near_cursor' }
  },
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ '/', '?' }, {
  completion = {
    autocomplete = false
  },
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  completion = {
    autocomplete = false
  },
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

-- Set up lspconfig.
local capabilities = require('cmp_nvim_lsp').default_capabilities()
-- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
require('lspconfig').tsserver.setup({
  capabilities = capabilities,
  on_attach = function(client)
    client.server_capabilities.semanticTokensProvider = nil
  end
})

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

-- UltiSnips config
g.UltiSnipsSnippetsDir = '~/.config/nvim/UltiSnips'
g.UltiSnipsExpandTrigger = '<c-j>'
g.UltiSnipsJumpForwardTrigger = '<Plug>(ultisnips_jump_forward)'
g.UltiSnipsJumpBackwardTrigger = '<Plug>(ultisnips_jump_backward)'
g.UltiSnipsListSnippets = '<c-x><c-s>'
g.UltiSnipsRemoveSelectModeMappings = 0

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

-- nvim-highlight-colors config
require('nvim-highlight-colors').setup({})
