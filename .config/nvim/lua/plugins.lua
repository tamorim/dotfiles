table.unpack = table.unpack or unpack

local g = vim.g
local opt = vim.opt
local fn = vim.fn
local api = vim.api
local env = vim.env
local lsp = vim.lsp

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
  {
    'folke/tokyonight.nvim',
    lazy = false,
    priority = 1000,
    opts = {
      style = 'storm',
      styles = {
        comments = { italic = false },
        keywords = { italic = false },
      },
    },
  },
  {
    'nvim-treesitter/nvim-treesitter',
    priority = 100,
    branch = 'master',
    lazy = false,
    build = ':TSUpdate',
    init = function()
      require('nvim-treesitter.configs').setup({
        ensure_installed = {
          'c',
          'lua',
          'luadoc',
          'vim',
          'vimdoc',
          'query',
          'bash',
          'markdown',
          'markdown_inline',
          'json',
          'yaml',
          'toml',
          'csv',
          'xml',
          'html',
          'css',
          'scss',
          'javascript',
          'jsdoc',
          'typescript',
          'tsx',
          'regex',
          'styled',
          'dockerfile',
          'nginx',
        },
        sync_install = false,
        auto_install = false,
        highlight = {
          enable = true,
          -- Disable slow treesitter highlight for large files
          disable = function(_, buf)
            local max_filesize = 100 * 1024 -- 100 KB
            local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
            if ok and stats and stats.size > max_filesize then
              return true
            end
          end,
        },
        indent = {
          enable = true,
        },
      })
    end,
  },
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

  -- Completion
  {
    'hrsh7th/cmp-nvim-lsp',
    dependencies = {
      'neovim/nvim-lspconfig',
      'luals/lua-language-server',
    },
    init = function()
      local capabilities = require('cmp_nvim_lsp').default_capabilities()

      local vtslsFileTypesConfig = {
        preferences = {
          includePackageJsonAutoImports = 'off',
        },
        suggest = {
          autoImports = true,
          includeCompletionsForImportStatements = true,
        },
      }
      lsp.config('vtsls', {
        capabilities = capabilities,
        settings = {
          typescript = vtslsFileTypesConfig,
          javascript = vtslsFileTypesConfig,
          vtsls = {
            tsserver = {
              globalPlugins = {
                {
                  name = '@styled/typescript-styled-plugin',
                  location = '~/.volta/tools/shared',
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

      lsp.config('lua_ls', {
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
            runtime = {
              version = 'LuaJIT',
              path = {
                'lua/?.lua',
                'lua/?/init.lua',
              },
            },
            workspace = {
              checkThirdParty = false,
              library = {
                env.VIMRUNTIME,
                '${3rd}/luv/library',
              },
            },
          },
        },
        on_attach = function(client)
          client.server_capabilities.semanticTokensProvider = nil
        end,
      })

      lsp.config('tailwindcss', {
        capabilities = capabilities,
        settings = {
          tailwindCSS = {
            classAttributes = {
              'class',
              'className',
              'class:list',
              'classList',
              'ngClass',
              'tw',
              'css',
            },
            experimental = {
              classRegex = {
                'tw`([^`]*)',
                'tw="([^"]*)',
                'tw={"([^"}]*)',
                'tw\\.\\w+`([^`]*)',
                'tw\\(.*?\\)`([^`]*)',
              },
            },
          },
        },
        on_attach = function(client)
          client.server_capabilities.semanticTokensProvider = nil
        end,
      })

      lsp.enable({ 'lua_ls', 'vtsls', 'tailwindcss' })
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

  -- Formatters and linters
  'editorconfig/editorconfig-vim',
  {
    'stevearc/conform.nvim',
    dependencies = { 'hrsh7th/nvim-cmp' },
    opts = {
      formatters_by_ft = {
        lua = { 'stylua' },
        javascript = { 'eslint_d', 'prettierd' },
        javascriptreact = { 'eslint_d', 'prettierd' },
        typescript = { 'eslint_d', 'prettierd' },
        typescriptreact = { 'eslint_d', 'prettierd' },
        css = { 'prettierd' },
        markdown = { 'prettierd' },
        json = { 'prettierd' },
        html = { 'prettierd' },
      },
    },
    init = function()
      local conform = require('conform')

      api.nvim_create_autocmd({ 'BufWritePre' }, {
        pattern = { '*.js', '*.jsx', '*.ts', '*.tsx' },
        group = api.nvim_create_augroup('conform_format_eslint_d', { clear = true }),
        callback = function(args)
          conform.format({ bufnr = args.buf, formatters = { 'eslint_d' } })
        end,
      })

      api.nvim_create_autocmd({ 'BufWritePre' }, {
        pattern = { '*.js', '*.jsx', '*.ts', '*.tsx', '*.json', '*.md', '*.html', '*.css' },
        group = api.nvim_create_augroup('conform_format_prettierd', { clear = true }),
        callback = function(args)
          conform.format({ bufnr = args.buf, formatters = { 'prettierd' } })
        end,
      })

      api.nvim_create_autocmd({ 'BufWritePre' }, {
        pattern = '*.lua',
        group = api.nvim_create_augroup('conform_format_stylua', { clear = true }),
        callback = function(args)
          conform.format({ bufnr = args.buf, formatters = { 'stylua' } })
        end,
      })
    end,
  },
  {
    'mfussenegger/nvim-lint',
    init = function()
      local lint = require('lint')
      local cmp = require('cmp')

      lint.linters_by_ft = {
        javascript = { 'eslint_d' },
        typescript = { 'eslint_d' },
        javascriptreact = { 'eslint_d' },
        typescriptreact = { 'eslint_d' },
      }

      cmp.event:on('confirm_done', function()
        local current_buffer = api.nvim_get_current_buf()
        local filetype = api.nvim_get_option_value('filetype', { buf = current_buffer })

        if
          filetype == 'javascript'
          or filetype == 'javascriptreact'
          or filetype == 'typescript'
          or filetype == 'typescriptreact'
        then
          lint.try_lint()
        end
      end)

      api.nvim_create_autocmd({ 'BufEnter', 'TextChanged', 'TextChangedI', 'BufWritePre' }, {
        pattern = '*',
        group = api.nvim_create_augroup('nvim_lint', { clear = true }),
        callback = function()
          lint.try_lint()
        end,
      })
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
            '-g',
            '!.yarn/',
            '-g',
            '!.git/',
            '-g',
            '!node_modules/',
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
              '-g',
              '!.yarn/',
              '-g',
              '!.git/',
              '-g',
              '!node_modules/',
            },
          },
          diagnostics = {
            wrap_results = true,
          },
          lsp_references = {
            show_line = false,
          },
        },
      })
      telescope.load_extension('fzf')
    end,
  },
  {
    'nvim-tree/nvim-tree.lua',
    opts = {
      view = {
        width = 45,
      },
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
        colorscheme = 'tokyonight',
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
    init = function()
      g.mkdp_auto_close = 0
      g.mkdp_browser = [[/Applications/Google Chrome.app]]
      g.mkdp_echo_preview_url = 1
      g.mkdp_page_title = '${name}'
      g.mkdp_theme = 'light'
    end,
  },
}, {
  ui = {
    -- The backdrop opacity. 0 is fully opaque, 100 is fully transparent.
    backdrop = 100,
  },
})
