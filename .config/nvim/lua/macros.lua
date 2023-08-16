table.unpack = table.unpack or unpack

local fn = vim.fn

-- Macro for visualizing blocks
fn.setreg('v', 'V$%')

-- Macro for navigating blocks
fn.setreg('n', '$%')
