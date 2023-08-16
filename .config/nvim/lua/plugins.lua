table.unpack = table.unpack or unpack

local g = vim.g
local opt = vim.opt
local opt_local = vim.opt_local
local fn = vim.fn
local cmd = vim.cmd
local api = vim.api
local env = vim.env
local keymap = vim.keymap

local lazypath = fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable',
    lazypath,
  })
end
opt.rtp:prepend(lazypath)

require('lazy').setup({
  -- Color scheme and syntax highlight
  { 'drewtempelmeyer/palenight.vim', priority = 1000 },
  { 'sheerun/vim-polyglot', priority = 100 },
  { 'brenoprata10/nvim-highlight-colors', opts = {} },

  -- Git
  {
    'tpope/vim-fugitive',
    init = function()
      api.nvim_create_autocmd('BufReadPost', {
        pattern = 'fugitive://*',
        group = api.nvim_create_augroup('custom_fugitive', { clear = true }),
        command = 'set bufhidden=delete',
      })
    end,
  },
  {
    'tpope/vim-rhubarb',
    dependencies = { 'tpope/vim-fugitive' },
  },
  'junegunn/gv.vim',
  'airblade/vim-gitgutter',

  -- Enhancements
  'tpope/vim-sleuth',
  'tpope/vim-surround',
  'tpope/vim-rsi',
  'tpope/vim-unimpaired',
  'tpope/vim-repeat',
  'Raimondi/delimitMate',
  'tomtom/tcomment_vim',
  'maxbrunsfeld/vim-yankstack',
  'osyo-manga/vim-over',
  {
    'mattn/emmet-vim',
    init = function()
      g.user_emmet_settings = {
        ['javascript.jsx'] = { extends = 'jsx' },
      }
    end,
  },

  -- Completion and lint
  'neovim/nvim-lspconfig',
  {
    'hrsh7th/cmp-nvim-lsp',
    dependencies = { 'neovim/nvim-lspconfig' },
    init = function()
      local capabilities = require('cmp_nvim_lsp').default_capabilities()
      require('lspconfig').tsserver.setup({
        capabilities = capabilities,
        on_attach = function(client)
          client.server_capabilities.semanticTokensProvider = nil
        end,
      })
    end,
  },
  {
    'dcampos/nvim-snippy',
    opts = {
      mappings = {
        i = {
          ['<C-j>'] = 'expand',
          ['<C-n>'] = 'next',
          ['<C-p>'] = 'previous',
        },
      },
    },
  },
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-nvim-lsp-signature-help',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-cmdline',
      { 'dcampos/cmp-snippy', dependencies = { 'dcampos/nvim-snippy' } },
    },
    opts = function()
      local snippy = require('snippy')
      local cmp = require('cmp')

      cmp.setup.cmdline({ '/', '?' }, {
        completion = {
          autocomplete = false,
        },
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'buffer' },
        },
      })

      cmp.setup.cmdline(':', {
        completion = {
          autocomplete = false,
        },
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = 'path' },
        }, {
          { name = 'cmdline' },
        }),
      })

      local get_select_behavior = function()
        if snippy.is_active() then
          return cmp.SelectBehavior.Select
        else
          return cmp.SelectBehavior.Insert
        end
      end

      return {
        snippet = {
          expand = function(args)
            snippy.expand_snippet(args.body)
          end,
        },
        mapping = {
          ['<Tab>'] = function(fallback)
            if cmp.visible() then
              cmp.select_next_item({ behavior = get_select_behavior() })
            else
              fallback()
            end
          end,
          ['<S-Tab>'] = function(fallback)
            if cmp.visible() then
              cmp.select_prev_item({ behavior = get_select_behavior() })
            else
              fallback()
            end
          end,
          ['<C-b>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<C-e>'] = cmp.mapping.abort(),
          ['<CR>'] = cmp.mapping.confirm({ select = false }),
        },
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          { name = 'nvim_lsp_signature_help' },
          { name = 'snippy' },
        }, {
          { name = 'buffer' },
          { name = 'path' },
        }),
        view = {
          entries = { name = 'custom', selection_order = 'near_cursor' },
        },
      }
    end,
  },
  {
    'mfussenegger/nvim-lint',
    init = function()
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
        end,
      })
    end,
  },

  -- Formatters
  'editorconfig/editorconfig-vim',
  {
    'prettier/vim-prettier',
    build = 'yarn install',
    init = function()
      g['prettier#autoformat'] = 0
      g['prettier#quickfix_enabled'] = 0
      local has_prettier_rc = fn.filereadable(fn.fnamemodify('.prettierrc', ':p'))
      local has_prettier_js = fn.filereadable(fn.fnamemodify('prettier.config.js', ':p'))
      local has_prettier_config = has_prettier_rc or has_prettier_js
      if has_prettier_config then
        api.nvim_create_autocmd('BufWritePre', {
          pattern = { '*.js', '*.jsx', '*.ts', '*.tsx', '*.css', '*.json', '*.md', '*.html' },
          group = api.nvim_create_augroup('prettier', { clear = true }),
          command = 'Prettier',
        })
      end
    end,
  },
  {
    'ckipp01/stylua-nvim',
    build = 'npm i -g @johnnymorganz/stylua-bin',
    init = function()
      local has_stylua_config = fn.filereadable(fn.fnamemodify('stylua.toml', ':p'))
      local has_dot_stylua_config = fn.filereadable(fn.fnamemodify('.stylua.toml', ':p'))
      if has_stylua_config or has_dot_stylua_config then
        api.nvim_create_autocmd('BufWritePre', {
          pattern = '*.lua',
          group = api.nvim_create_augroup('stylua', { clear = true }),
          callback = function()
            require('stylua-nvim').format_file()
          end,
        })
      end
    end,
  },

  -- Misc
  {
    'junegunn/fzf.vim',
    dependencies = {
      {
        'junegunn/fzf',
        build = function()
          fn['fzf#install']()
        end,
        lazy = false,
      },
    },
    init = function()
      g.fzf_layout = { down = '40%' }
      g.rg_command =
        'rg --fixed-strings --column --line-number --no-heading --ignore-case --hidden --follow --color=always --ignore-file ~/.agignore '
      api.nvim_create_user_command(
        'Rg',
        'call fzf#vim#grep(g:rg_command.shellescape(<q-args>), 1, <bang>0)',
        { bang = true, nargs = '*' }
      )
    end,
  },
  {
    'justinmk/vim-dirvish',
    init = function()
      g.dirvish_mode = ':sort ,^.*/,'
    end,
  },
  {
    'itchyny/lightline.vim',
    init = function()
      g.lightline = {
        colorscheme = 'palenight',
        active = {
          right = {
            { 'lineinfo' },
            { 'percent' },
            { 'filetype' },
          },
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
          t = 'T',
        },
      }
    end,
  },
  { 'shime/vim-livedown', ft = 'markdown' },
})
