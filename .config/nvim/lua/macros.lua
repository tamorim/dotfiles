table.unpack = table.unpack or unpack

local g = vim.g
local opt = vim.opt
local opt_local = vim.opt_local
local fn = vim.fn
local cmd = vim.cmd
local api = vim.api
local env = vim.env
local keymap = vim.keymap

-- Macro for visualizing blocks
fn.setreg('v', 'V$%')

-- Macro for navigating blocks
fn.setreg('n', '$%')
