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
  use('tpope/vim-fugitive')
  use('junegunn/gv.vim')
  use('tpope/vim-rhubarb')
  use('tpope/vim-rsi')
  use('tpope/vim-unimpaired')
  use('mattn/emmet-vim')
  use('itchyny/lightline.vim')
  use('Raimondi/delimitMate')
  use('airblade/vim-gitgutter')
  use('tpope/vim-sleuth')
  use('tomtom/tcomment_vim')
  use('tpope/vim-repeat')
  use('sheerun/vim-polyglot')
  use('editorconfig/editorconfig-vim')
  use('neovim/nvim-lspconfig')
  use('hrsh7th/cmp-nvim-lsp')
  use('hrsh7th/cmp-nvim-lsp-signature-help')
  use('hrsh7th/cmp-buffer')
  use('hrsh7th/cmp-path')
  use('hrsh7th/cmp-cmdline')
  use('hrsh7th/nvim-cmp')
  use({ 'SirVer/ultisnips', config = function()
    g.UltiSnipsExpandTrigger = '<Plug>(ultisnips_expand)'
    g.UltiSnipsJumpForwardTrigger = '<Plug>(ultisnips_jump_forward)'
    g.UltiSnipsJumpBackwardTrigger = '<Plug>(ultisnips_jump_backward)'
    g.UltiSnipsListSnippets = '<c-x><c-s>'
    g.UltiSnipsRemoveSelectModeMappings = 0
  end })
  use('quangnguyen30192/cmp-nvim-ultisnips')
  use('mfussenegger/nvim-lint')
  use('/opt/homebrew/opt/fzf')
  use('junegunn/fzf.vim')
  use('drewtempelmeyer/palenight.vim')
  use('maxbrunsfeld/vim-yankstack')
  use('tpope/vim-surround')
  use({ 'shime/vim-livedown', ft = 'markdown' })
  use('osyo-manga/vim-over')
  use('justinmk/vim-dirvish')
  use({ 'prettier/vim-prettier', run = 'yarn install' })

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
