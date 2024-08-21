-- Pull in the wezterm API
local wezterm = require('wezterm')
local act = wezterm.action
local mux = wezterm.mux

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

config.front_end = 'WebGpu'
config.webgpu_power_preference = 'HighPerformance'

-- fonts
config.font = wezterm.font_with_fallback({
  'JetBrains Mono',
  'Apple Color Emoji',
  'Symbols Nerd Font Mono',
})
config.font_size = 14.0
config.freetype_load_target = 'Light'
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }

-- colors
config.color_scheme = 'Palenight (Gogh)'

-- keybindings
config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }
config.keys = {
  { key = '-', mods = 'LEADER', action = act.SplitVertical({ domain = 'CurrentPaneDomain' }) },
  { key = '/', mods = 'LEADER', action = act.SplitHorizontal({ domain = 'CurrentPaneDomain' }) },
  { key = 'x', mods = 'LEADER', action = wezterm.action.CloseCurrentPane({ confirm = true }) },
  { key = 'z', mods = 'LEADER', action = act.TogglePaneZoomState },
  { key = '[', mods = 'LEADER', action = act.ActivateCopyMode },
  { key = 'h', mods = 'LEADER', action = act.ActivatePaneDirection('Left') },
  { key = 'l', mods = 'LEADER', action = act.ActivatePaneDirection('Right') },
  { key = 'k', mods = 'LEADER', action = act.ActivatePaneDirection('Up') },
  { key = 'j', mods = 'LEADER', action = act.ActivatePaneDirection('Down') },
  { key = 'r', mods = 'LEADER', action = act.ActivateKeyTable({ name = 'resize_pane', one_shot = false }) },
  { key = 'b', mods = 'LEADER', action = act.RotatePanes('CounterClockwise') },
  { key = 'n', mods = 'LEADER', action = act.RotatePanes('Clockwise') },
  {
    key = 'T',
    mods = 'LEADER',
    action = act.PromptInputLine({
      description = 'Enter new name for tab',
      action = wezterm.action_callback(function(window, _, line)
        -- line will be `nil` if they hit escape without entering anything
        -- An empty string if they just hit enter
        -- Or the actual line of text they wrote
        if line then
          window:active_tab():set_title(line)
        end
      end),
    }),
  },
}

config.key_tables = {
  -- Defines the keys that are active in our resize-pane mode.
  -- Since we're likely to want to make multiple adjustments,
  -- we made the activation one_shot=false. We therefore need
  -- to define a key assignment for getting out of this mode.
  -- 'resize_pane' here corresponds to the name="resize_pane" in
  -- the key assignments above.
  resize_pane = {
    { key = 'h', action = act.AdjustPaneSize({ 'Left', 5 }) },
    { key = 'j', action = act.AdjustPaneSize({ 'Down', 5 }) },
    { key = 'k', action = act.AdjustPaneSize({ 'Up', 5 }) },
    { key = 'l', action = act.AdjustPaneSize({ 'Right', 5 }) },
    -- Cancel the mode by pressing escape
    { key = 'Escape', action = act.PopKeyTable },
  },
}

-- maximize on gui startup
wezterm.on('gui-startup', function(cmd)
  local _, _, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

-- and finally, return the configuration to wezterm
return config
