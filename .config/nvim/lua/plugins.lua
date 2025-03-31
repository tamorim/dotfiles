table.unpack = table.unpack or unpack

local g = vim.g
local opt = vim.opt
local fn = vim.fn
local api = vim.api
local env = vim.env

local lazypath = fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.uv.fs_stat(lazypath) then
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
  {
    'windwp/nvim-autopairs',
    event = 'InsertEnter',
    config = true,
  },
  {
    'numToStr/Comment.nvim',
    config = true,
  },
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
  {
    'hrsh7th/cmp-nvim-lsp',
    dependencies = {
      'neovim/nvim-lspconfig',
      'luals/lua-language-server',
    },
    init = function()
      local lspconfig = require('lspconfig')
      local capabilities = require('cmp_nvim_lsp').default_capabilities()

      local vtslsFileTypesConfig = {
        preferences = {
          includePackageJsonAutoImports = 'off',
        },
        suggest = {
          autoImports = false,
          includeCompletionsForImportStatements = false,
        },
      }
      lspconfig.vtsls.setup({
        capabilities = capabilities,
        settings = {
          typescript = vtslsFileTypesConfig,
          javascript = vtslsFileTypesConfig,
          vtsls = {
            tsserver = {
              globalPlugins = {
                {
                  name = '@styled/typescript-styled-plugin',
                  location = env.NVM_GLOBAL_MODULES_DIR,
                  enableForWorkspaceTypeScriptVersions = true,
                },
              },
            },
          },
        },
        on_attach = function(client)
          client.server_capabilities.semanticTokensProvider = nil
        end,
      })

      lspconfig.lua_ls.setup({
        capabilities = capabilities,
        cmd = {
          'lua-language-server',
          '--logpath',
          '~/.cache/lua-language-server/',
          '--metapath',
          '~/.cache/lua-language-server/meta/',
        },
        settings = {
          Lua = {
            runtime = { version = 'LuaJIT' },
            workspace = {
              checkThirdParty = false,
              library = {
                vim.env.VIMRUNTIME,
                '${3rd}/luv/library',
              },
            },
          },
        },
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
      'hrsh7th/cmp-nvim-lua',
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
        performance = {
          debounce = 100,
          fetching_timeout = 100,
        },
        matching = {
          disallow_fuzzy_matching = true,
          disallow_fullfuzzy_matching = true,
          disallow_partial_fuzzy_matching = true,
          disallow_partial_matching = false,
          disallow_prefix_unmatching = true,
        },
        sorting = {
          comparators = {
            cmp.config.compare.exact,
            cmp.config.compare.offset,
            cmp.config.compare.score,
            cmp.config.compare.locality,
            cmp.config.compare.scopes,
            cmp.config.compare.recently_used,
            cmp.config.compare.kind,
          },
        },
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
          { name = 'nvim_lsp', max_item_count = 30 },
          { name = 'nvim_lsp_signature_help' },
          { name = 'snippy' },
          { name = 'nvim_lua' },
        }, {
          { name = 'buffer', indexing_interval = 1000 },
          { name = 'path' },
        }),
      }
    end,
  },
  {
    'mfussenegger/nvim-lint',
    init = function()
      vim.env.ESLINT_D_PPID = fn.getpid()
      require('lint').linters_by_ft = {
        javascript = { 'eslint_d' },
        typescript = { 'eslint_d' },
        javascriptreact = { 'eslint_d' },
        typescriptreact = { 'eslint_d' },
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
    'nvim-telescope/telescope.nvim',
    tag = '0.1.8',
    dependencies = {
      'nvim-lua/plenary.nvim',
      { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
    },
    init = function()
      local telescope = require('telescope')
      telescope.setup({
        defaults = {
          path_display = { 'truncate' },
          layout_config = {
            horizontal = {
              width = 0.99,
            },
          },
          vimgrep_arguments = {
            'rg',
            '--color=never',
            '--no-heading',
            '--with-filename',
            '--line-number',
            '--column',
            '--smart-case',
            '--hidden',
            '--ignore-file',
            '~/.rgignore',
            '-g',
            '!yarn.lock',
            '-g',
            '!package-lock.json',
          },
        },
        pickers = {
          find_files = {
            find_command = {
              'rg',
              '--files',
              '--color',
              'never',
              '--hidden',
              '--ignore-file',
              '~/.rgignore',
            },
          },
          diagnostics = {
            wrap_results = true,
          },
          lsp_references = {
            path_display = { 'shorten' },
            fname_width = 45,
          },
        },
      })
      telescope.load_extension('fzf')
    end,
  },
  {
    'nvim-tree/nvim-tree.lua',
    opts = {
      filesystem_watchers = {
        enable = true,
        debounce_delay = 50,
        ignore_dirs = {
          'node_modules',
        },
      },
      renderer = {
        add_trailing = true,
        icons = {
          show = {
            file = false,
            folder = false,
            folder_arrow = false,
            git = false,
            modified = false,
            diagnostics = false,
            bookmarks = false,
          },
        },
      },
      filters = {
        git_ignored = false,
      },
      update_focused_file = {
        enable = true,
      },
    },
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
  {
    'iamcco/markdown-preview.nvim',
    cmd = { 'MarkdownPreviewToggle', 'MarkdownPreview', 'MarkdownPreviewStop' },
    ft = { 'markdown' },
    build = function()
      vim.fn['mkdp#util#install']()
    end,
  },
}, {
  ui = {
    -- The backdrop opacity. 0 is fully opaque, 100 is fully transparent.
    backdrop = 100,
  },
})
