table.unpack = table.unpack or unpack

local g = vim.g
local opt = vim.opt
local opt_local = vim.opt_local
local fn = vim.fn
local cmd = vim.cmd
local api = vim.api
local env = vim.env
local keymap = vim.keymap

local M = {}

-- Set tabstop, softtabstop and shiftwidth to the same value
function M.summarize_tabs()
  local tabs = {
    tabstop = vim.opt_local.tabstop:get(),
    shiftwidth = vim.opt_local.shiftwidth:get(),
    softtabstop = vim.opt_local.softtabstop:get(),
    expandtab = tostring(vim.opt_local.expandtab:get())
  }
  local message = string.gsub('tabstop = $tabstop, shiftwidth = $shiftwidth, softtabstop = $softtabstop, expandtab = $expandtab', '%$(%w+)', tabs)
  print(message)
end

function M.stab()
  local tabstop = tonumber(fn.input('set tabstop = softtabstop = shiftwidth = '))
  if tabstop ~= nil and tabstop > 0 then
    opt_local.softtabstop = tabstop
    opt_local.tabstop = tabstop
    opt_local.shiftwidth = tabstop
  end
  summarize_tabs()
end

-- Organize range by length
function M.sort_lines()
  local first_line = fn.getpos('v')[2]
  local last_line = fn.getpos('.')[2]
  cmd([[silent! execute ']] .. first_line .. ',' .. last_line .. [[s/^\(.*\)$/\=strdisplaywidth(submatch(0)) . " " . submatch(0)/']])
  cmd([[silent! execute ']] .. first_line .. ',' .. last_line .. [[sort n']])
  cmd([[silent! execute ']] .. first_line .. ',' .. last_line .. [[s/^\d\+\s//']])
end

-- Indent a React component's jsx code
function M.indent_react()
  cmd([[silent! execute 's/\v\<\w+\zs\s\ze|\zs\s\ze\w+\=|("|})\zs\s\ze\w+/\="\n" . matchstr(getline("."), ''^\s*'') . "  "/g']])
  cmd([[silent! execute 's/\v\s?(\/?\>)/\="\n" . matchstr(getline("."), ''^\s*'') . submatch(1)/']])
  cmd.normal('<<')
  cmd([[silent! execute 's/\v\zs(\>)\ze.+/\=submatch(1) . "\n" . matchstr(getline("."), ''^\s*'')/']])
  cmd.normal('>>')
  cmd([[silent! execute 's/\v(\<\/\w+\>)$/\="\n" . matchstr(getline("."), ''^\s*'') . submatch(1)/']])
  cmd.normal('<<')
end

-- Why is this not a built-in Vim script function?!
function M.get_visual_selection()
  local visual_start = fn.getpos('v')
  local line_start = visual_start[2]
  local column_start = visual_start[3]
  local visual_end = fn.getpos('.')
  local line_end = visual_end[2]
  local column_end = visual_end[3]
  local lines = fn.getline(line_start, line_end)
  if fn.len(lines) == 0 then
      return ''
  end
  if opt.selection:get() == 'inclusive' then
    lines[#lines] = string.sub(lines[#lines], 1, column_end)
  else
    lines[#lines] = string.sub(lines[#lines], 1, column_end - 1)
  end
  lines[1] = string.sub(lines[1], column_start - 1)
  return fn.join(lines, [[\n]])
end

-- Indent a long javascript object, array or parameter list
function M.indent_list()
  local delimiter_map = { ['{'] = '}', ['['] = ']', ['('] = ')' }
  local selection = get_visual_selection()
  local start_delimiters = fn.join(fn.keys(delimiter_map), [[\|]])
  local start_delimiter = fn.matchstr(selection, start_delimiters)
  local end_delimiter = delimiter_map[start_delimiter]
  local space = fn.matchstr(fn.getline('v'), [[\v^(\s*)]])
  local step1 = fn.substitute(
   selection,
   [[\v.{-}\]] .. start_delimiter .. [[\s?]],
   [[\="]] .. start_delimiter .. [[\r]] .. space .. [[  "]],
   ''
  )
  local step2 = fn.substitute(
    step1,
    [[\v.{-},\zs\s?\ze]],
    [[\="\r]] .. space .. [[  "]],
    'g'
  )
  local step3 = fn.substitute(
    step2,
    [[\v.{-}\zs\s?\]] .. end_delimiter .. [[(.*\]] .. end_delimiter .. [[)@!.*\ze]],
    [[\="\r]] .. space .. [[\]] .. end_delimiter .. [["]],
    ''
  )

  cmd([[silent! execute 's/\v.{-}\zs\]] .. start_delimiter .. [[.{-}\]] .. end_delimiter .. [[(.*\]] .. end_delimiter .. [[)@!\ze/]] .. step3 .. [[']])
end

function M.create_file_or_dir()
  local file_or_dir = fn.input('New file or dir name: ')
  if #file_or_dir == 0 then
    return
  end
  if string.sub(file_or_dir, -1) == '/' then
    cmd([[silent! execute '!mkdir -p ]] .. fn.expand('%') .. file_or_dir .. [[']])
  else
    cmd([[silent! execute '!touch ]] .. fn.expand('%') .. file_or_dir .. [[']])
  end
  cmd.normal('R')
end

function M.remove_file_or_dir()
  local current_file = fn.getline('.')
  cmd([[silent! execute '!rm -rf ]] .. current_file .. [[']])
  cmd.normal('R')
end

function M.copy_file_or_dir()
  local current_file = fn.getline('.')
  local destination = fn.input('Copy destination: ', current_file)
  local flag
  if fn.match(current_file, [[/$]]) > -1 then
    flag = ' -R '
  else
    flag = ' '
  end
  if fn.strchars(destination) == 0 then
    return
  end
  cmd([[silent! execute '!cp]] .. flag .. current_file .. ' ' .. destination .. [[']])
  cmd.normal('R')
end

function M.move_file_or_dir()
  local current_file = fn.getline('.')
  local destination = fn.input('Move destination: ', current_file)
  if fn.strchars(destination) == 0 then
    return
  end
  cmd([[silent! execute '!mv ]] .. current_file .. ' ' .. destination .. [[']])
  cmd.normal('R')
end

function M.push_git_branch_to_origin(no_verify, force)
  local current_branch = fn.system([[git branch | grep -e '^*' | tr -d '*' | tr -cd '[:print:]']])
  if not no_verify and not force then
    cmd.execute('Git --paginate ps -u ' .. current_branch)
  elseif no_verify and not force then
    cmd.execute('Git --paginate ps -u --no-verify ' .. current_branch)
  elseif not no_verify and force then
    fn.execute('Git --paginate ps -u --force ' .. current_branch)
  elseif no_verify and force then
    fn.execute('Git --paginate ps -u --no-verify --force ' .. current_branch)
  end
end

function M.refresh_gv()
  fn.execute('bdelete')
  fn.execute('GV')
end

function M.commit_fixup_to_current_sha()
  fn.execute('Git commit --fixup=' .. fn['gv#sha']())
  refresh_gv()
end

function M.interactive_rebase_with_current_sha()
  fn.execute('Git rebase -i ' .. fn['gv#sha']())
  refresh_gv()
end

function M.auto_squash_rebase_with_current_sha()
  fn.execute('Git rebase --autosquash ' .. fn['gv#sha']())
  refresh_gv()
end

function M.search_with_current_word()
  fn.execute('Rg ' .. fn.expand('<cword>'))
end

function M.search_with_current_selection()
  local current_selection = get_visual_selection()
  fn.execute('Rg ' .. current_selection)
end

return M
