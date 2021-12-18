-- Aliases
local cmd = vim.cmd  -- to execute Vim commands e.g. cmd('pwd')
local fn = vim.fn    -- to call Vim functions e.g. fn.bufnr()
local g = vim.g      -- a table to access global variables
local opt = vim.opt  -- to set options

local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Plugins
require "paq" {
    "savq/paq-nvim";
    "nvim-treesitter/nvim-treesitter";
    "neovim/nvim-lspconfig";
    "windwp/nvim-autopairs";
    "b3nj5m1n/kommentary";
}

-- Options
opt.expandtab = true                -- Use spaces instead of tabs
opt.hidden = true                   -- Enable background buffers
opt.hlsearch = true                 -- Highlight search terms
opt.ignorecase = true               -- Ignore case
opt.joinspaces = false              -- No double spaces with join
opt.list = true                     -- Show some invisible characters
opt.number = false                   -- Show line numbers
opt.scrolloff = 4                   -- Lines of context
opt.shiftround = true               -- Round indent
opt.shiftwidth = 2                  -- Size of an indent
opt.sidescrolloff = 8               -- Columns of context
opt.smartcase = true                -- Do not ignore case with capitals
opt.smartindent = true              -- Insert indents automatically
opt.splitbelow = true               -- Put new windows below current
opt.splitright = true               -- Put new windows right of current
opt.expandtab = true
opt.tabstop = 2                     -- Number of spaces tabs count for
opt.termguicolors = true            -- True color support
opt.wildmode = {'list', 'longest'}  -- Command-line completion mode
opt.wrap = false                    -- Disable line wrap
opt.mouse = 'a'
opt.undofile = true                 -- persistent undo
opt.clipboard = "unnamedplus"
g.mapleader = " "

-- Mappings
map('n', '<Esc>', '<cmd>noh<CR>')    -- Clear highlights

-- Tree-sitter
local ts = require 'nvim-treesitter.configs'
ts.setup {ensure_installed = 'maintained', highlight = {enable = true}}
